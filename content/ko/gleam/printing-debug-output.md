---
title:                "Gleam: 디버그 출력하기"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 출력하는 것이 왜 중요한지 궁금하신가요? 실제 코드에서 이해하기 어려운 버그를 찾을 때, 디버그 출력은 매우 유용합니다. 여러분만의 코드에서도 디버그 출력을 이용해서 문제를 신속하게 해결할 수 있습니다.

## 어떻게

Gleam에서 디버그 출력을 사용하는 방법은 아주 간단합니다. 다음과 같이 `debug_io.print` 함수를 사용할 수 있습니다.

```Gleam
let name = "John"
debug_io.print("Hello, " ++ name ++ "!")  
```

위의 코드를 실행하면 다음과 같은 출력이 나옵니다.

```
Hello, John!
```

따옴표(`"`) 사이에 직접 내용을 넣거나 변수를 이용해 출력 내용을 지정할 수 있습니다. 또한 여러 줄의 디버그 출력을 한 번에 출력하거나, 조건문과 함께 사용하여 특정 조건에서만 디버그 출력이 나오도록 할 수도 있습니다.

## 딥 다이브

디버그 출력을 사용하는 방법 이외에도, 디버그 출력을 어떻게 사용할 수 있는지에 대해 더 알아보겠습니다. 디버그 출력은 코드의 실행 흐름을 확인할 수 있는 좋은 도구입니다. 코드의 어느 부분에서 어떤 값을 가지고 있는지 확인하거나, 코드가 실행되는 과정에서 어떤 문제가 발생하는지 파악할 수 있습니다. 디버그 출력을 통해 최종적으로 버그를 찾아 해결하는 데 매우 유용합니다.

## 참고

- [Gleam 공식 문서 - 디버깅](https://gleam.run/book/tour/debugging.html)
- [여정자님 블로그 - 디버그 출력 사용하기](https://blog.yegeonjae.com/463)
- [Elixir 공식 문서 - 디버그 출력](https://hexdocs.pm/elixir/IO.html#inspect/2)