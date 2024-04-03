---
date: 2024-01-19
description: "YAML (YAML Ain't Markup Language)\uC740 \uB370\uC774\uD130\uB97C \uD45C\
  \uD604\uD558\uAE30 \uC704\uD55C \uC778\uAC04 \uCE5C\uD654\uC801\uC778 \uD615\uC2DD\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC124\uC815 \uD30C\
  \uC77C, \uB370\uC774\uD130 \uAD50\uD658 \uB4F1\uC5D0 \uC27D\uAC8C \uC77D\uACE0 \uC4F8\
  \ \uC218 \uC788\uB3C4\uB85D \uD558\uAE30 \uC704\uD574 YAML\uC744 \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.139952-06:00'
model: unknown
summary: "YAML (YAML Ain't Markup Language)\uC740 \uB370\uC774\uD130\uB97C \uD45C\uD604\
  \uD558\uAE30 \uC704\uD55C \uC778\uAC04 \uCE5C\uD654\uC801\uC778 \uD615\uC2DD\uC785\
  \uB2C8\uB2E4."
title: "YAML \uB2E4\uB8E8\uAE30"
weight: 41
---

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
