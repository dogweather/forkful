---
title:                "Elm: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜

YAML을 다루는 것에 대한 이유는 무엇일까요? YAML은 데이터 형식이며 더 나은 구조와 가독성을 제공합니다. Elm에서 YAML을 작업하면 코드를 더 잘 구조화할 수 있고 더 쉽게 이해할 수 있습니다.

## 어떻게

YAML을 Elm에서 작업하기 위해서는 먼저 YAML 패키지를 설치해야 합니다. `elm install elm-explorations/markdown` 명령어를 사용하면 쉽게 설치할 수 있습니다.

YAML을 파일에서 읽어오려면 `yaml` 패키지의 `decode` 함수를 사용합니다. 그 다음, 다음과 같이 코드를 작성합니다.

```Elm
import Yaml exposing (decode)
import Html

main =
    Html.text "Welcome to my blog about YAML in Elm!"
```

그리고 다음과 같이 YAML 파일을 읽어올 수 있습니다.

```Elm
yamlString = """
name: John
age: 25
city: Seoul
"""

decodedYaml =
    decode yamlString

main =
    case decodedYaml of
        Ok yaml ->
            Html.text ("Name: " ++ yaml.name ++ " Age: " ++ (toString yaml.age) ++ " City: " ++ yaml.city)

        Err error ->
            Html.text ("Error decoding YAML: " ++ (toString error))
```

위 코드를 실행하면 다음과 같은 결과가 나옵니다.

```
Name: John Age: 25 City: Seoul
```

## 딥 다이브

YAML은 유연하고 간단한 데이터 형식입니다. 더 많은 정보를 알고 싶다면 YAML 공식 웹사이트와 yaml 패키지의 문서를 참고하시기 바랍니다. 또한 `elm-explorations/markdown` 패키지를 사용하면 Markdown을 YAML에서 읽어올 수 있습니다.

## 참고

- YAML 공식 웹사이트: https://yaml.org/
- yaml 패키지 문서: https://package.elm-lang.org/packages/elm-explorations/yaml/latest/
- elm-explorations/markdown 패키지 문서: https://package.elm-lang.org/packages/elm-explorations/markdown/latest/