---
title:                "Elm: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Elm 프로그래밍을 하고 계신가요? 그렇다면 이 블로그 포스트는 여러분을 위한 것입니다. 오늘은 Elm에서 문자열을 대문자로 변환하는 것에 대해 알아보겠습니다. 왜 이 기능이 필요한지, 어떻게 사용하는지 그리고 더 깊이 들어가서 더 자세히 살펴보도록 하겠습니다.

## 왜 대문자로 변환하나요?

Elm에서는 텍스트를 입력 받을 때 대문자와 소문자를 구분합니다. 하지만 상황에 따라 모든 문자들을 대문자로 변환하는 것이 필요할 수 있습니다. 예를 들면 사용자의 입력에 대한 유효성 검사를 할 때, 입력된 문자열을 모두 대문자로 변환해야 한다면 이 기능이 필요합니다.

## 사용 방법

이제 실제로 Elm 코드를 통해 문자열을 대문자로 변환하는 방법을 알아보겠습니다. 먼저 `import String`을 사용하여 문자열을 다루는 함수를 가져옵니다. 그리고 `String.toUpper` 함수를 사용하여 입력된 문자열을 대문자로 변환할 수 있습니다.

```elm
import String

String.toUpper "hello" -- "HELLO"
String.toUpper "elm" -- "ELM"
```

위의 예시 코드에서 볼 수 있듯이 `toUpper` 함수는 입력된 문자열을 모두 대문자로 변환합니다. 또한, 대문자를 포함한 문자열을 입력하더라도 동일한 결과가 출력됩니다.

## 더 자세히 살펴보기

지금까지는 간단하게 문자열을 대문자로 변환하는 방법을 알아봤지만, 더 자세히 살펴보도록 하겠습니다. Elm에서는 Unicode 문자도 지원하기 때문에 다국어를 다룰 때 유용하게 사용될 수 있습니다. 또한, `toUpper` 함수만 있는 것이 아니라 `toLower` 함수로 모든 문자를 소문자로 변환하는 것도 가능합니다.

## 다른 함수와 비교하기

Elm 코드를 작성하면서 대문자로 변환할 때 `String.toUpper` 함수를 사용하는 것이 가장 쉽고 간단한 방법입니다. 하지만 다른 언어에서는 어떤 함수를 사용하는지 살펴보도록 하겠습니다.

- JavaScript: `toUpperCase()` 메소드
- Python: `upper()` 메소드
- Java: `toUpperCase()` 메소드

## 작동 원리

Elm에서 `String.toUpper` 함수는 `Char` 타입의 리스트를 이용하여 문자열을 대문자로 변환합니다. 각 `Char`는 해당하는 문자에 대한 대문자 코드 값을 가지고 있습니다.

## See Also

이제 대문자로 변환하는 법을 배웠으니 다른 문자열을 다루는 함수들을 살펴보세요. [Elm 문자열 관련 문서](https://package.elm-lang.org/packages/elm-lang/core/latest/String)에서 `String` 모듈을 더 자세히 살펴볼 수 있습니다. 또한, [공식 Elm 웹사이트](https://elm-lang.org/)에서 다른 기능들을 알아보세요.