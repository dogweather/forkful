---
title:                "Elm: 디버그 출력 출력"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

코드 디버깅을 위해 디버그 출력을 활용하는 이유는 코드 내부를 분석하고 오류를 찾아내기 위해서입니다.

## 어떻게

```elm
-- `Debug.log` 함수를 사용하여 디버그 출력을 생성할 수 있습니다.
main : Html msg
main =
  let
    name = "Elm"
    age = 5
  in
    Debug.log "name" name -- "name: Elm"
    Debug.log "age" age -- "age: 5"
    Html.text "Hello World"
```

위의 코드에서는 `Debug.log` 함수를 사용하여 변수 `name`과 `age`의 값을 확인하고 디버그 출력을 생성합니다. 코드 실행 시 콘솔 창에서 다음과 같은 결과를 볼 수 있습니다.

```
name: Elm
age: 5
```

## 딥 다이브

디버그 출력을 사용할 때 유의해야 할 몇 가지 핵심적인 사항이 있습니다. 

첫 번째로, 디버그 출력을 남겨놓은 상태로 제출하지 말아야 합니다. 디버그 출력은 오로지 코드 디버깅이나 테스트를 위해 사용하는 것이며, 실제로는 제출되는 코드에 영향을 미치지 않아야 합니다.

또한, 디버그 출력이나 코드 디버깅의 지속적인 사용은 성능을 저하시킬 수 있습니다. 따라서 디버그 출력은 오직 코드 디버깅을 위해 잠깐 사용되어야 합니다. 

## 또 다른 정보

[Elm Official Website](https://elm-lang.org/) <br>
[Elm Debug Module Documentation](https://package.elm-lang.org/packages/elm/debug/latest/Debug) <br>
[Debugging in Elm: Console.log vs. Debug.log](https://medium.com/@ckoster22/debugging-in-elm-console-log-vs-debug-log-2877dae799dc)