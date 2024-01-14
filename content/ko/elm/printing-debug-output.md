---
title:    "Elm: 디버그 출력 출력하기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

왜 개발자들이 디버그 출력을 출력할까요? 이것은 개발과정에서 발생하는 오류를 파악하고 해결하는데 도움을 줄 수 있기 때문입니다.

## 출력하는 방법

Elm 언어에서 디버그 출력을 하는 가장 간단한 방법은 `Debug.log` 함수를 사용하는 것입니다. 아래의 예제 코드를 참고해보세요.

```Elm
import Debug exposing (log)

myFunction : String -> String
myFunction name =
    let
        message = "Hello, " ++ name
    in
        message |> log "Function Message"
```

위의 예제에서는 `Debug.log` 함수를 사용하여 `message` 변수 값을 "Function Message" 라는 레이블과 함께 출력하고 있습니다. 실제 실행결과는 아래와 같이 보일 것 입니다.

```
Function Message: Hello, Kristine
```

## 깊숙한 이론

디버그 출력은 개발과정에서 중요한 도구입니다. 디버그 출력을 사용하여 어떤 변수의 값이 어떻게 변하는지 추적하거나, 함수 실행 중 발생하는 오류를 파악할 수 있습니다. 또한 Elm 언어에서는 `Debug.watch`나 `Debug.crash`와 같은 다양한 디버그 함수를 제공하고 있습니다. 이러한 함수들을 잘 이용하면 코드의 디버깅 과정을 효율적으로 할 수 있습니다.

## 더 보기

- [Elm 공식 문서 - 디버깅과 출력](https://guide.elm-lang.org/development/debugging.html)
- [Elm Debug 패키지 - 사용법 및 예제](https://package.elm-lang.org/packages/elm-lang/core/latest/Debug)
- [예제로 배우는 Elm 프로그래밍 - 디버그와 디버그 함수 소개](https://elm-primer.oopsno.dev/#side-effects-debugging-and-debug-functions)