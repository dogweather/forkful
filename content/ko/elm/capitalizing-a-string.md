---
title:                "Elm: 문자열 대문자로 바꾸기"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 바꾸는 이유는 우리가 프로그래밍에서 자주 사용하는 일반적인 작업이기 때문입니다. 예를 들어, 사용자의 입력을 강제로 대문자로 바꾸거나, 데이터베이스에 저장되는 문자열을 보다 일관성있게 처리하기 위해서 사용할 수 있습니다. 여러분이 이미 어떤 일을하든 간에, 문자열을 대문자로 변환하는 기능은 매우 유용합니다!

## 이번에는 어떻게

Elm으로 문자열을 대문자로 바꾸는 방법은 매우 간단합니다. 먼저, `String` 모듈을 가져와서 `toUpper` 함수를 사용하면 됩니다. 예제 코드를 살펴보겠습니다.

```Elm
import String exposing (toUpper)

myString = "hello world"
uppercaseString = toUpper myString

-- "HELLO WORLD"
```

코드를 살펴보면, `toUpper` 함수는 문자열을 인자로 받고 대문자로 변환된 값을 리턴합니다. 따라서 `uppercaseString` 변수에는 `HELLO WORLD`라는 대문자로 된 문자열이 저장되게 됩니다.

## 깊은 곳까지 파헤쳐보기

이제 문자열을 대문자로 바꾸는 것은 얼마나 쉽고 간단한 작업인지 알게 되었습니다. 하지만 우리는 여전히 더 자세히 살펴볼 수 있는 것들이 있습니다.

우선, `String` 모듈은 `toUpper` 함수 외에도 여러 가지 다른 함수들을 제공합니다. 예를 들어, `toLower` 함수는 문자열을 소문자로 변환하는 역할을 합니다. 또한 `toCapitalize` 함수는 각 단어의 첫 글자를 대문자로 변환합니다.

또한, `String` 모듈을 사용할 때에는 몇 가지 중요한 점들을 명심해야 합니다. 예를 들어, `toUpper` 함수는 영어 외의 다른 언어에서는 적용되지 않을 수 있으므로 주의해야 합니다.

## 관련 포스트 보기

- [Elm 문자열 함수 문서](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm 문자열 기초 강좌](https://scrimba.com/playlist/p5nMr9){target="_blank"}
- [Elm 한국어 커뮤니티](https://www.facebook.com/groups/ElmKorea){target="_blank"}