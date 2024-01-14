---
title:                "Elixir: 디버그 출력 출력하기"
simple_title:         "디버그 출력 출력하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 프린트하는 이유는 코드에서 문제를 식별하고 수정하기 위해서 필요합니다. 이를 통해 코드의 로직을 이해하고 디버깅하는 데 도움이 됩니다.

## 하는 방법

```Elixir
IO.inspect(some_variable)
```
위와 같이 `IO.inspect` 함수를 사용하여 변수의 값을 출력할 수 있습니다. 또는 `IO.puts`를 사용하여 문자열을 출력할 수도 있습니다. 아래는 Elixir 코드에서 디버그 출력을 하는 예제입니다.

```Elixir
defmodule ExampleModule do
  def some_function(some_argument) do
    IO.inspect(some_argument)
    some_processed_argument = some_argument + 1
    IO.puts("Processed argument: #{some_processed_argument}")
  end
end

ExampleModule.some_function(10)
```
위 코드의 출력 결과는 다음과 같을 것입니다.
```
10
Processed argument: 11
```

## 딥 다이브

디버그 출력은 코드를 이해하는 데 매우 중요합니다. 이를 통해 프로그램의 흐름을 추적하고 변수의 값과 상태를 파악할 수 있습니다. 또한 디버그 출력을 이용하면 어떤 함수가 어떤 변수에 접근하는지를 확인하는 등 디버깅을 할 때 매우 유용합니다.

## 관련 자료

- [Elixir 디버깅 가이드](https://elixir-lang.org/getting-started/debugging.html)
- [IO 모듈 공식 문서](https://hexdocs.pm/elixir/IO.html)
- [Elixir의 `IO.inspect`와 `IO.puts` 공식 문서](https://hexdocs.pm/elixir/IO.html#inspect/2)