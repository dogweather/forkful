---
title:                "YAML 다루기"
aliases: - /ko/elm/working-with-yaml.md
date:                  2024-01-19
simple_title:         "YAML 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
YAML (YAML Ain't Markup Language)은 데이터를 표현하기 위한 인간 친화적인 형식입니다. 프로그래머들은 설정 파일, 데이터 교환 등에 쉽게 읽고 쓸 수 있도록 하기 위해 YAML을 사용합니다.

## How to: (어떻게 사용 하나요?)
Elm에서는 YAML을 직접적으로 다루는 내장 라이브러리가 없어요. 그래서 JavaScript와 연동하여 사용할 수 있는데, `port`와 `decoder`를 활용해보세요. 아래는 그 예제입니다.

```Elm
port module Main exposing (..)

import Html exposing (Html, button, div, text)
import Json.Decode exposing (decodeValue)
import Json.Decode.Pipeline exposing (required)
import Ports exposing (yamlDecoder)

type alias User =
    { name : String
    , age : Int
    }

userDecoder : Json.Decode.Decoder User
userDecoder =
    decodeValue
        (Json.Decode.Pipeline.required "name" Json.Decode.string
            (Json.Decode.Pipeline.required "age" Json.Decode.int
                Json.Decode.succeed User
            )
        )

port getYaml : (String -> msg) -> Sub msg

port sendYaml : String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions _ =
    getYaml GotYaml

type Msg
    = GotYaml String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotYaml yamlString ->
            case userDecoder yamlString of
                Ok user ->
                    ( { model | user = Just user }, Cmd.none )
                Err _ ->
                    (model, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (sendYaml "{name: 'Jin', age: 25}") ] [ text "Get User Data" ] 
        , div [] [ text (maybe "No user data" (\user -> user.name) model.user) ]
        ]
```

이 Elm 코드는 JavaScript와 연동되어야 하며, 해당 YAML 문자열을 처리하기 위해 외부에서 지원되는 함수를 필요로 합니다.

```JavaScript
// 예: JavaScript 코드
app.ports.sendYaml.subscribe(function(yaml) {
    try {
        var result = YAML.parse(yaml);
        app.ports.getYaml.send(result);
    } catch (e) {
        console.error("Parsing failed!", e);
    }
});
```

## Deep Dive (심화 학습)
YAML은 2001년에 등장했어요. JSON과 비교할 때, 가독성이 뛰어나지만 파싱 과정이 복잡할 수 있습니다. Elm에서는 YAML을 직전단계인 JSON으로 변환해서 다루는 것이 일반적입니다. 이 경우, `yaml`을 `json`으로 변환하는 JavaScript 라이브러리가 필요해요.

## See Also (함께 보기)
- YAML 공식 사이트: [https://yaml.org/](https://yaml.org/)
- JavaScript YAML 파서 (js-yaml) [https://github.com/nodeca/js-yaml](https://github.com/nodeca/js-yaml)
- Elm의 JSON 디코딩: [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
