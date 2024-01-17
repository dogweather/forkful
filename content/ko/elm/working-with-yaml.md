---
title:                "yaml로 작업하기"
html_title:           "Elm: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## 어떤 것이고 왜 그래? 

YAML은 데이터를 저장하는 데 사용되는 일종의 파일 형식입니다. 프로그래머들은 YAML을 사용하여 구조화된 데이터를 더 쉽게 관리하고 사용할 수 있습니다.

## 이렇게 하는 법:

```Elm
-- YAML 라이브러리를 가져옵니다.
import JonnyBurger/elm-graphql/Yaml exposing (..)

-- YAML 파일을 읽어옵니다.
data : Result String Yaml.Value
data =
  Yaml.fromString """
    first_name: John
    last_name: Doe
  -- 이 공백 줄은 필요하지 않지만, 라이브러리에서 데이터를 파싱하기 위해 사용됩니다.
  """

-- 결과를 확인합니다!
case data of
  Ok yamlValue -> toString yamlValue -- "John Doe"를 반환합니다.
  Err errorMessage -> "오류가 발생했습니다: " ++ errorMessage
```

## 깊이 파고들기:

YAML은 "YAML Ain't Markup Language"의 약자입니다. 마크업 언어가 아니라는 이름에서 알 수 있듯이, 이 파일 형식은 구조화된 데이터를 저장하는 데 사용됩니다. 다른 대안으로는 XML 또는 JSON이 있지만, YAML은 더 직관적이고 읽기 쉽습니다. 또한 Elm에서 YAML을 읽어오기 위해서는 자바스크립트 라이브러리를 사용해야 합니다.

## 더 알아보기:

[YAML 공식 웹사이트](https://yaml.org/)

[Elm에서 자바스크립트 코드 사용하기](https://guide.elm-lang.org/interop/javascript.html)

[XML vs JSON vs YAML: 어떤 것을 사용할까요?](https://atlantbh.com/2018/10/05/xml-vs-json-vs-yaml/)