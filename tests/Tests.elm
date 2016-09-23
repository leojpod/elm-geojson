module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import GeoJson exposing (GeoJsonObject(..), Geometry(..), decoder)
import Json.Decode exposing (decodeString)


all : Test
all =
    concat [ expectedFailures, geometryExamples ]


expectErr : Result a b -> Expectation
expectErr r =
    case r of
        Ok _ ->
            Expect.fail <| "Expected an Err but got " ++ toString r

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


geometryExamples : Test
geometryExamples =
    let
        geomTest name { json, expected } =
            test name <|
                \() ->
                    decodeString decoder json
                        |> Expect.equal (Ok ( Geometry expected, Nothing, Nothing ))
    in
        describe "Geometry Examples from Appendix A of the specification"
            [ geomTest "Point"
                { json =
                    """{ "type": "Point", "coordinates": [100.0, 0.0] }"""
                , expected =
                    Point ( 100, 0, [] )
                }
            , geomTest "LineString"
                { json =
                    """
                       { "type": "LineString",
                        "coordinates": [ [100.0, 0.0], [101.0, 1.0] ]
                       }
                   """
                , expected =
                    LineString [ ( 100, 0, [] ), ( 101, 1, [] ) ]
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
                    Polygon [ [ ( 100, 0, [] ), ( 101, 0, [] ), ( 101, 1, [] ), ( 100, 1, [] ), ( 100, 0, [] ) ] ]
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
                        [ [ ( 100, 0, [] ), ( 101, 0, [] ), ( 101, 1, [] ), ( 100, 1, [] ), ( 100, 0, [] ) ]
                        , [ ( 100.2, 0.2, [] ), ( 100.8, 0.2, [] ), ( 100.8, 0.8, [] ), ( 100.2, 0.8, [] ), ( 100.2, 0.2, [] ) ]
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
                        [ ( 100, 0, [] ), ( 101, 1, [] ) ]
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
                        [ [ ( 100, 0, [] ), ( 101, 1, [] ) ]
                        , [ ( 102, 2, [] ), ( 103, 3, [] ) ]
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
                        [ [ [ ( 102, 2, [] ), ( 103, 2, [] ), ( 103, 3, [] ), ( 102, 3, [] ), ( 102, 2, [] ) ] ]
                        , [ [ ( 100, 0, [] ), ( 101, 0, [] ), ( 101, 1, [] ), ( 100, 1, [] ), ( 100, 0, [] ) ], [ ( 100.2, 0.2, [] ), ( 100.8, 0.2, [] ), ( 100.8, 0.8, [] ), ( 100.2, 0.8, [] ), ( 100.2, 0.2, [] ) ] ]
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
                        [ Point ( 100, 0, [] )
                        , LineString [ ( 101, 0, [] ), ( 102, 1, [] ) ]
                        ]
                }
            ]