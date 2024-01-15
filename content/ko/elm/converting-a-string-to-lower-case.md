---
title:                "스트링을 소문자로 변환하기"
html_title:           "Elm: 스트링을 소문자로 변환하기"
simple_title:         "스트링을 소문자로 변환하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것의 의의는 어떤 것인가요? 그 이유는 간단합니다. 현재 버전의 Elm은 대소문자를 가리지 않는 프로그래밍 언어이기 때문입니다. 따라서, 문자열을 소문자로 변환하는 것은 일관성 있고 깔끔한 코드 작성을 가능하게 합니다.

## 사용 방법

우선, 문자열을 소문자로 변환하기 위해서는 `String` 모듈을 가져와야 합니다. 그리고 `String.toLower` 함수를 호출하여 해당 문자열을 소문자로 변환합니다. 아래는 간단한 예제와 실행 결과입니다.

```Elm
import String exposing (toLower)

result = toLower "Hello, WORLD!"

-- 결과: "hello, world!"
```

더 많은 예제를 살펴보겠습니다. 아래는 `List.map` 를 활용하여 리스트 내의 모든 문자열을 소문자로 변환하는 예제입니다.

```Elm
import String exposing (toLower)

fruits = ["APPLE", "ORANGE", "BANANA"]

result = List.map toLower fruits

-- 결과: ["apple", "orange", "banana"]
```

위와 같이 `String.toLower` 함수는 입력받은 문자열의 모든 문자를 소문자로 변환하여 반환합니다.

## 깊은 곳으로

Elm의 `String.toLower` 함수는 어떻게 동작할까요? 이 함수는 영어 알파벳 기준으로만 동작합니다. 따라서, 다른 언어의 알파벳은 변환되지 않습니다. 또한, 공백, 숫자, 특수 문자 등은 그대로 남아있습니다. 예를 들어, `"안녕하세요!"` 라는 문자열을 `String.toLower` 함수로 변환하면 `"안녕하세요!"` 로 그대로 출력됩니다.

## 더 알아보기

- [Elm 공식 문서 - String 모듈](https://elm-lang.org/docs/official/latest/strings)
- [elm-live를 활용한 Elm 개발 환경 구축하기](https://khan.github.io/elm-live/)
- [프로그래밍 언어 Elm 소개 및 기초 문법 알아보기](https://blog.scottlogic.com/2018/10/05/learning-elm-intro-and-basic-types.html)

## 참고 자료

- [The official Elm guide](https://guide.elm-lang.org/)
- [Elm 커뮤니티 Slack 채널](https://elmlang.herokuapp.com/)
- [Elm 공식 홈페이지](https://elm-lang.org/)