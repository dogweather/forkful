---
title:                "디버그 출력 출력"
html_title:           "Elm: 디버그 출력 출력"
simple_title:         "디버그 출력 출력"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 만들기 위해 누군가 노력하는 이유는 무엇일까요? 디버그 출력은 프로그램의 실행을 분석하고 이해하는 데 도움을 줄 수 있습니다. 예를 들어, 코드가 실행되는 동안 변수의 값이 어떻게 변하는지 확인하거나 오류 메시지를 통해 문제를 찾는 데 도움을 줄 수 있습니다.

## 하는 방법

디버그 출력을 만드는 것은 매우 간단합니다. 먼저, `Debug` 모듈을 불러와야 합니다. 그런 다음, `Debug.log` 함수를 사용하여 원하는 내용을 디버그 출력으로 출력할 수 있습니다. 아래는 예제 코드와 출력의 의미를 보여주는 예시입니다.

```Elm
module Main exposing (main)

import Debug exposing (log)

main =
  log "Hello, World!"
```

위 코드를 실행하면 "Hello, World!"라는 메시지를 디버그 출력으로 확인할 수 있습니다. 이것은 `Debug.log` 함수를 사용하여 "Hello, World!"라는 텍스트를 출력했기 때문입니다. 다음은 콘솔에 출력된 내용입니다.

```
  "Hello, World!"
```

이제 변수의 값을 디버그 출력으로 확인하는 것도 매우 쉽습니다. 아래 예제 코드에는 `x`라는 정수 변수가 있고, 이 변수의 값이 어떻게 변하는지 확인하고 싶다고 가정해 봅시다.

```Elm
module Main exposing (main)

import Debug exposing (log)

main =
  let
    x = 5
  in
    log x
```

위 코드를 실행하면 `x`의 값인 5가 디버그 출력으로 확인됩니다.

```
5
```

디버그 출력은 한 번에 하나의 값만 출력할 수 있기 때문에 두 개 이상의 값을 확인하려면 `..` 연산자를 사용하여 값들을 묶어야 합니다. 아래 예제 코드는 `x`와 `y`라는 두 개의 정수 변수를 출력하는 방법을 보여줍니다.

```Elm
module Main exposing (main)

import Debug exposing (log)

main =
  let
    x = 5
    y = 10
  in
    log (x, y)
```

위 코드를 실행하면 `x`와 `y`의 값인 5와 10이 묶여서 디버그 출력으로 확인됩니다.

```
(5, 10)
```

## 더 깊게 들어가기

디버그 출력에는 다양한 옵션들이 있습니다. 예를 들어, 출력되는 값을 포맷팅하거나 로그의 레벨을 지정할 수 있습니다. 자세한 정보는 [Elm 공식 문서](https://package.elm-lang.org/packages/elm/core/latest/Debug)를 참조하시기 바랍니다.

## 더 알아보기

- [Elm 공식 문서: Debug 모듈](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Elm-Debug package (디버깅 툴)](https://package.elm-lang.org/packages/elm/debug/latest/)