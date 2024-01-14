---
title:    "Elm: 문자열을 소문자로 변환하기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜

변수를 전부 소문자로 변환하는 것이 왜 유용한지를 알고 싶으신가요? 그럼 계속 읽어보세요!

## 어떻게 해야 할까요

문자열을 소문자로 변환하는 것은 간단합니다. Elm의 `String` 모듈에 이미 내장된 `toLower` 함수를 사용하면 됩니다. 아래는 이 함수를 사용한 예시 코드입니다.

```Elm
import String

lowercaseString = String.toLower "ELM PROGRAMMING"
```

위 코드의 실행 결과는 다음과 같습니다.

```Elm
"elm programming"
```

## 깊이 들어가기

문자열을 소문자로 변환하는 이유는 여러 가지가 있습니다. 첫 번째로는 일관성을 유지하기 위해서입니다. 대부분의 프로그래밍 언어는 대/소문자를 구분하므로, 변수명이나 함수명을 정할 때 일관성을 유지하는 것이 중요합니다. 또한 문자열 내용을 비교할 때도 대소문자를 구분하기 때문에, 문자열을 소문자로 변환하면 비교가 더 쉬워집니다.

그렇다면 문자열을 대문자로 변환하는 방법은 없을까요? Elm의 `String` 모듈에는 `toUpper` 함수도 있는데, 이는 `toLower` 함수와 사용법이 같습니다.

# 더 알아보기

문자열을 소문자로 변환하는 것 외에도 여러 가지 유용한 기능이 있는 Elm의 `String` 모듈에 대해 더 알고 싶다면 아래 링크를 참고해보세요.

## 함께 보기

- [Elm String 모듈 문서](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm 공식 홈페이지](https://elm-lang.org/)