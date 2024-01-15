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

## 왜
이 글을 읽고 YAML과 함께 작업하는 것에 참여하는 이유는 무엇일까요? YAML은 구조화된 데이터를 저장하고 전송하는 데 유용한 포맷이며, Elm에서도 매우 쉽게 다룰 수 있습니다.

## 어떻게
아래는 YAML을 Elm에서 다루는 간단한 예제 코드와 실행 결과입니다.

```Elm
import YAML exposing (..)

-- YAML 데이터를 Elm 타입으로 디코딩
type alias Person = 
    { name : String
    , age : Int
    , occupation : String
    }

yamlData : String
yamlData = 
    """
        name: John
        age: 25
        occupation: Software Engineer
    """

decodedPerson : Result YAMLError Person
decodedPerson = decode yamlData

-- 디코딩 결과 확인
case decodedPerson of
    Ok person -> 
        "이름: " ++ person.name ++ ", 나이: " ++ (toString person.age) ++ ", 직업: " ++ person.occupation

    Err error -> 
        "디코딩 실패: " ++ (toString error)
```

```
결과: 이름: John, 나이: 25, 직업: Software Engineer
```

## 깊게 들어가기
YAML은 정적 타입 시스템을 가진 Elm에서 빠르고 안전하게 다룰 수 있는 형식입니다. Elm의 종속 타입으로 YAML 데이터를 디코딩하면, 다양한 형태로 변환할 수 있습니다. 또한, Elm에서 YAML을 사용해 데이터를 저장하고 가공할 수 있습니다.

## 더 알아보기
[YAML 공식 문서](https://yaml.org/)\
[Elm-YAML 패키지 페이지](https://package.elm-lang.org/packages/jreut/elm-yaml/latest/YAML)\
[Elm 공식 문서](https://elm-lang.org/docs)