module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import GeoJson exposing (Bbox, CustomGeoJson, FeatureObject, GeoJson, GeoJsonObject(..), Geometry(..), Position, decoder)
import Json.Decode as D exposing (decodeString)
import Json.Encode as E
import Test exposing (..)
import Time exposing (Posix)


type alias TimeProps =
    { time : Posix }


type alias TimedGeoJson =
    CustomGeoJson TimeProps


propsDecoder : D.Decoder TimeProps
propsDecoder =
    D.field "time" (D.map (Time.millisToPosix >> TimeProps) D.int)


encodeProps : TimeProps -> E.Value
encodeProps { time } =
    E.object [ ( "time", E.int <| Time.posixToMillis time ) ]


emptyObject : E.Value
emptyObject =
    E.object []


encodeAndDecode : Test
encodeAndDecode =
    fuzzWith { runs = 30, distribution = noDistribution } fuzzGeoJson "encoding and decoding does not change the GeoJson" <|
        \geojson ->
            geojson
                |> GeoJson.encode
                |> D.decodeValue GeoJson.decoder
                |> Expect.equal (Ok geojson)


encodeAndDecodeCustom : Test
encodeAndDecodeCustom =
    fuzzWith { runs = 30, distribution = noDistribution } fuzzTimedGeoJson "encoding and decoding does not change the CustomGeoJson" <|
        \geojson ->
            geojson
                |> GeoJson.encodeCustom encodeProps
                |> D.decodeValue (GeoJson.customDecoder propsDecoder)
                |> Expect.equal (Ok geojson)


fuzzGeoJson : Fuzzer GeoJson
fuzzGeoJson =
    Fuzz.pair
        (fuzzGeoJsonObject (Fuzz.constant emptyObject))
        (Fuzz.maybe fuzzBbox)


fuzzTimedGeoJson : Fuzzer (CustomGeoJson { time : Posix })
fuzzTimedGeoJson =
    Fuzz.pair
        (fuzzGeoJsonObject fuzzTimeProps)
        (Fuzz.maybe fuzzBbox)


fuzzGeoJsonObject : Fuzzer props -> Fuzzer (GeoJsonObject props)
fuzzGeoJsonObject propsFuzzer =
    Fuzz.frequency
        [ ( 1, Fuzz.map FeatureCollection (Fuzz.list (fuzzFeature propsFuzzer)) )
        , ( 1, Fuzz.map Feature (fuzzFeature propsFuzzer) )
        , ( 7, Fuzz.map Geometry fuzzGeometry )
        ]


fuzzFeature : Fuzzer props -> Fuzzer (FeatureObject props)
fuzzFeature propsFuzzer =
    Fuzz.map3 (\geom props id -> FeatureObject geom props id)
        (Fuzz.maybe fuzzGeometry)
        propsFuzzer
        (Fuzz.maybe Fuzz.string)


fuzzGeometry : Fuzzer Geometry
fuzzGeometry =
    let
        helper depth =
            Fuzz.frequency
                [ ( 1, Fuzz.map Point fuzzPosition )
                , ( 1, Fuzz.map MultiPoint (Fuzz.list fuzzPosition) )
                , ( 1, Fuzz.map LineString (Fuzz.list fuzzPosition) )
                , ( 1, Fuzz.map MultiLineString (Fuzz.list (Fuzz.list fuzzPosition)) )
                , ( 1, Fuzz.map Polygon (Fuzz.list (Fuzz.list fuzzPosition)) )
                , ( 1, Fuzz.map (\xs -> MultiPolygon [ xs ]) (Fuzz.list (Fuzz.list fuzzPosition)) )
                , if depth > 2 then
                    ( 0, Fuzz.constant (Point ( 1, 2, 0 )) )

                  else
                    ( 1 / depth, Fuzz.map GeometryCollection (Fuzz.list (helper (depth + 1))) )
                ]
    in
    helper 1


fuzzPosition : Fuzzer Position
fuzzPosition =
    Fuzz.frequency
        [ ( 1, Fuzz.map2 (\a b -> ( a, b, 0 )) Fuzz.niceFloat Fuzz.niceFloat )
        , ( 1
          , Fuzz.map3
                (\a b c ->
                    let
                        c_ =
                            if c == 0 then
                                1

                            else
                                c
                    in
                    ( a, b, c_ )
                )
                Fuzz.niceFloat
                Fuzz.niceFloat
                Fuzz.niceFloat
          )
        ]


fuzzBbox : Fuzzer Bbox
fuzzBbox =
    Fuzz.pair
        (Fuzz.pair Fuzz.niceFloat Fuzz.niceFloat)
        (Fuzz.pair Fuzz.niceFloat Fuzz.niceFloat)
        |> Fuzz.map (\( ( a, b ), ( c, d ) ) -> [ a, b, c, d ])


fuzzTimeProps : Fuzzer { time : Posix }
fuzzTimeProps =
    Fuzz.intAtLeast 0
        |> Fuzz.map (\millis -> { time = Time.millisToPosix millis })


expectErr : Result a b -> Expectation
expectErr r =
    case r of
        Ok _ ->
            Expect.fail <| "Expected an Err but got " ++ Debug.toString r

        Err _ ->
            Expect.pass


expectedFailures : Test
expectedFailures =
    describe "Invalid GeoJSON"
        [ test "Invalid type" <|
            \() ->
                let
                    json =
                        """{"type": "NotAnActualType"}"""
                in
                decodeString decoder json |> expectErr
        , test "No coordinates" <|
            \() ->
                let
                    json =
                        """{"type": "Point"}"""
                in
                decodeString decoder json |> expectErr
        , test "Not enough indices in position " <|
            \() ->
                let
                    json =
                        """{"type": "Point", "coordinates": [1]}"""
                in
                decodeString decoder json |> expectErr
        ]


featureObjectTests : Test
featureObjectTests =
    describe "feature objects"
        [ test "No properties fails" <|
            \() ->
                let
                    json =
                        """{"type": "Feature", "geometry": null}"""
                in
                decodeString decoder json |> expectErr
        , test "id can be a absent" <|
            \() ->
                let
                    json =
                        """{"type": "Feature", "geometry": null, "properties": {}}"""

                    expected =
                        Ok ( Feature { geometry = Nothing, properties = emptyObject, id = Nothing }, Nothing )
                in
                decodeString decoder json |> Expect.equal expected
        , test "id can be a string" <|
            \() ->
                let
                    json =
                        """{"type": "Feature", "geometry": null, "properties": {}, "id": "foo"}"""

                    expected =
                        Ok ( Feature { geometry = Nothing, properties = emptyObject, id = Just "foo" }, Nothing )
                in
                decodeString decoder json |> Expect.equal expected
        , test "id can be an int" <|
            \() ->
                let
                    json =
                        """{"type": "Feature", "geometry": null, "properties": {}, "id": 42}"""

                    expected =
                        Ok ( Feature { geometry = Nothing, properties = emptyObject, id = Just "42" }, Nothing )
                in
                decodeString decoder json |> Expect.equal expected
        ]


geometryExamples : Test
geometryExamples =
    let
        geomTest name { json, expected } =
            test name <|
                \() ->
                    decodeString decoder json
                        |> Expect.equal (Ok ( Geometry expected, Nothing ))
    in
    describe "Geometry Examples"
        [ describe "from Appendix A of the 2008 specification"
            [ geomTest "Point"
                { json =
                    """{ "type": "Point", "coordinates": [100.0, 0.0] }"""
                , expected =
                    Point ( 100, 0, 0 )
                }
            , geomTest "LineString"
                { json =
                    """
                       { "type": "LineString",
                        "coordinates": [ [100.0, 0.0], [101.0, 1.0] ]
                       }
                   """
                , expected =
                    LineString [ ( 100, 0, 0 ), ( 101, 1, 0 ) ]
                }
            , geomTest "Polygon"
                { json =
                    """
                    { "type": "Polygon",
                      "coordinates": [
                         [ [100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0] ]
                       ]
                    }
                   """
                , expected =
                    Polygon [ [ ( 100, 0, 0 ), ( 101, 0, 0 ), ( 101, 1, 0 ), ( 100, 1, 0 ), ( 100, 0, 0 ) ] ]
                }
            , geomTest "Polygon with holes"
                { json =
                    """
                    { "type": "Polygon",
                      "coordinates": [
                         [ [100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0] ],
                         [ [100.2, 0.2], [100.8, 0.2], [100.8, 0.8], [100.2, 0.8], [100.2, 0.2] ]
                         ]
                      }
                    """
                , expected =
                    Polygon
                        [ [ ( 100, 0, 0 ), ( 101, 0, 0 ), ( 101, 1, 0 ), ( 100, 1, 0 ), ( 100, 0, 0 ) ]
                        , [ ( 100.2, 0.2, 0 ), ( 100.8, 0.2, 0 ), ( 100.8, 0.8, 0 ), ( 100.2, 0.8, 0 ), ( 100.2, 0.2, 0 ) ]
                        ]
                }
            , geomTest "MultiPoint"
                { json =
                    """
                    { "type": "MultiPoint",
                      "coordinates": [ [100.0, 0.0], [101.0, 1.0] ]
                    }
                    """
                , expected =
                    MultiPoint
                        [ ( 100, 0, 0 ), ( 101, 1, 0 ) ]
                }
            , geomTest "MultiLineString"
                { json =
                    """
                        { "type": "MultiLineString",
                          "coordinates": [
                            [ [100.0, 0.0], [101.0, 1.0] ],
                            [ [102.0, 2.0], [103.0, 3.0] ]
                          ]
                        }
                    """
                , expected =
                    MultiLineString
                        [ [ ( 100, 0, 0 ), ( 101, 1, 0 ) ]
                        , [ ( 102, 2, 0 ), ( 103, 3, 0 ) ]
                        ]
                }
            , geomTest "MultiPolygon"
                { json =
                    """
                        { "type": "MultiPolygon",
                          "coordinates": [
                              [[[102.0, 2.0], [103.0, 2.0], [103.0, 3.0], [102.0, 3.0], [102.0, 2.0]]],
                              [[[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0]],
                               [[100.2, 0.2], [100.8, 0.2], [100.8, 0.8], [100.2, 0.8], [100.2, 0.2]]]
                              ]
                        }
                    """
                , expected =
                    MultiPolygon
                        [ [ [ ( 102, 2, 0 ), ( 103, 2, 0 ), ( 103, 3, 0 ), ( 102, 3, 0 ), ( 102, 2, 0 ) ] ]
                        , [ [ ( 100, 0, 0 ), ( 101, 0, 0 ), ( 101, 1, 0 ), ( 100, 1, 0 ), ( 100, 0, 0 ) ], [ ( 100.2, 0.2, 0 ), ( 100.8, 0.2, 0 ), ( 100.8, 0.8, 0 ), ( 100.2, 0.8, 0 ), ( 100.2, 0.2, 0 ) ] ]
                        ]
                }
            , geomTest "GeometryCollection"
                { json =
                    """
                        { "type": "GeometryCollection",
                          "geometries": [
                            { "type": "Point",
                              "coordinates": [100.0, 0.0]
                            },
                            { "type": "LineString",
                              "coordinates": [ [101.0, 0.0], [102.0, 1.0] ]
                            }
                         ]
                      }
                    """
                , expected =
                    GeometryCollection
                        [ Point ( 100, 0, 0 )
                        , LineString [ ( 101, 0, 0 ), ( 102, 1, 0 ) ]
                        ]
                }
            ]
        , describe "from RFC 7946"
            [ test "Section 1.5" <|
                \_ ->
                    decodeString decoder
                        """
                    {
                       "type": "FeatureCollection",
                       "features": [{
                           "type": "Feature",
                           "geometry": {
                               "type": "Point",
                               "coordinates": [102.0, 0.5]
                           },
                           "properties": {
                               "prop0": "value0"
                           }
                       }, {
                           "type": "Feature",
                           "geometry": {
                               "type": "LineString",
                               "coordinates": [
                                   [102.0, 0.0],
                                   [103.0, 1.0],
                                   [104.0, 0.0],
                                   [105.0, 1.0]
                               ]
                           },
                           "properties": {
                               "prop0": "value0",
                               "prop1": 0.0
                           }
                       }, {
                           "type": "Feature",
                           "geometry": {
                               "type": "Polygon",
                               "coordinates": [
                                   [
                                       [100.0, 0.0],
                                       [101.0, 0.0],
                                       [101.0, 1.0],
                                       [100.0, 1.0],
                                       [100.0, 0.0]
                                   ]
                               ]
                           },
                           "properties": {
                               "prop0": "value0",
                               "prop1": {
                                   "this": "that"
                               }
                           }
                       }]
                   }
                   """
                        |> Expect.equal
                            (Ok
                                ( FeatureCollection
                                    [ { geometry = Just (Point ( 102, 0.5, 0 ))
                                      , properties = E.object [ ( "prop0", E.string "value0" ) ]
                                      , id = Nothing
                                      }
                                    , { geometry = Just (LineString [ ( 102, 0, 0 ), ( 103, 1, 0 ), ( 104, 0, 0 ), ( 105, 1, 0 ) ])
                                      , properties =
                                            E.object
                                                [ ( "prop0", E.string "value0" ), ( "prop1", E.int 0 ) ]
                                      , id = Nothing
                                      }
                                    , { geometry =
                                            Just
                                                (Polygon
                                                    [ [ ( 100, 0, 0 ), ( 101, 0, 0 ), ( 101, 1, 0 ), ( 100, 1, 0 ), ( 100, 0, 0 ) ] ]
                                                )
                                      , properties =
                                            E.object
                                                [ ( "prop0", E.string "value0" )
                                                , ( "prop1"
                                                  , E.object [ ( "this", E.string "that" ) ]
                                                  )
                                                ]
                                      , id = Nothing
                                      }
                                    ]
                                , Nothing
                                )
                            )
            , geomTest "Antimeridian cutting 1"
                { json =
                    """
                                    {
                                        "type": "MultiLineString",
                                        "coordinates": [
                                            [
                                                [170.0, 45.0], [180.0, 45.0]
                                            ], [
                                                [-180.0, 45.0], [-170.0, 45.0]
                                            ]
                                        ]
                                    }
                                    """
                , expected = MultiLineString [ [ ( 170, 45, 0 ), ( 180, 45, 0 ) ], [ ( -180, 45, 0 ), ( -170, 45, 0 ) ] ]
                }
            , geomTest "Antimeridian cutting 2"
                { json =
                    """
                        {
                            "type": "MultiPolygon",
                            "coordinates": [
                                [
                                    [
                                        [180.0, 40.0], [180.0, 50.0], [170.0, 50.0],
                                        [170.0, 40.0], [180.0, 40.0]
                                    ]
                                ],
                                [
                                    [
                                        [-170.0, 40.0], [-170.0, 50.0], [-180.0, 50.0],
                                        [-180.0, 40.0], [-170.0, 40.0]
                                    ]
                                ]
                            ]
                        }
                                    """
                , expected =
                    MultiPolygon
                        [ [ [ ( 180, 40, 0 ), ( 180, 50, 0 ), ( 170, 50, 0 ), ( 170, 40, 0 ), ( 180, 40, 0 ) ] ], [ [ ( -170, 40, 0 ), ( -170, 50, 0 ), ( -180, 50, 0 ), ( -180, 40, 0 ), ( -170, 40, 0 ) ] ] ]
                }
            , test "bounding box, section 5" <|
                \_ ->
                    decodeString decoder
                        """
                    {
                        "type": "Feature",
                        "bbox": [-10.0, -10.0, 10.0, 10.0],
                        "geometry": {
                            "type": "Polygon",
                            "coordinates": [
                                [
                                    [-10.0, -10.0],
                                    [10.0, -10.0],
                                    [10.0, 10.0],
                                    [-10.0, -10.0]
                                ]
                            ]
                        },
                        "properties": {}
                    }
                    """
                        |> Expect.equal
                            (Ok
                                ( Feature
                                    { geometry = Just (Polygon [ [ ( -10, -10, 0 ), ( 10, -10, 0 ), ( 10, 10, 0 ), ( -10, -10, 0 ) ] ])
                                    , properties = emptyObject
                                    , id = Nothing
                                    }
                                , Just [ -10, -10, 10, 10 ]
                                )
                            )
            ]
        ]
