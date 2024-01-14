---
title:    "Elixir: 디버그 출력 인쇄"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

Elixir 프로그래밍 블로그: 디버그 출력하는 방법

## 왜 디버그 출력을 하는가?

디버그 출력은 코드를 실행하고 디버깅하는 과정에서 매우 유용합니다. 코드가 실행되는 동안 변수의 값과 함수의 반환값을 확인할 수 있어 문제를 빠르게 발견하고 수정할 수 있습니다. 또한 코드의 흐름을 이해하는 데에도 도움이 됩니다.

## 디버그 출력하는 방법

디버그 출력은 Elixir에서 `IO.inspect/1` 함수를 사용하는 것으로 간단하게 할 수 있습니다. 이 함수는 입력값을 출력하고 그 값을 다시 반환합니다. 아래는 `IO.inspect/1` 함수를 사용한 예제 코드와 출력 내용입니다.

```Elixir
numbers = [1, 2, 3, 4, 5]
IO.inspect(numbers)
# 출력: [1, 2, 3, 4, 5]
```

함수나 모듈을 디버그할 때에는 함수 또는 모듈 이름을 `IO.inspect/2` 함수의 두 번째 인자로 넘겨줄 수도 있습니다. 이렇게 하면 해당 함수나 모듈의 코드 또는 출력 내용이 출력되어 디버깅을 보다 쉽게 할 수 있습니다. 아래는 `Enum.filter/2` 함수를 디버그할 때 사용하는 예제 코드와 출력 내용입니다.

```Elixir
defmodule MyModule do
  def double(numbers) do
    Enum.filter(numbers, & &1 * 2 =:= 6)
  end
end

IO.inspect(MyModule, :double)
# 출력:
# MyModule
#   double/1
#     [1, 2, 3, 4, 5]
```

## 디버그 출력에 대해 깊게 알아보기

디버그 출력을 하면서 유의해야 할 점은 너무 많은 값이나 정보를 출력하지 않도록 하는 것입니다. 너무 많은 값이나 정보를 출력하면 오히려 코드를 파악하기 어려워질 수 있습니다. 디버그 출력을 할 때에는 필요한 정보만 충분히 출력하고 출력을 위한 코드는 최대한 간단하게 유지하는 것이 좋습니다.

## 더 알아보기

- [IO.inspect/1](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Elixir Debugging](https://medium.com/nothingmorethannothing/elixir-debugging-fa545951d4ef)
- [Elixir Cookbook - Debugging](https://elixirschool.com/en/lessons/advanced/debugging/)

## 관련 링크

- [Elixir 공식 홈페이지](https://elixir-lang.org/)
- [Elixir 한국 커뮤니티](https://elixir-korea.org/)
- [Elixir 슬랙 채널](https://elixir-slackin.herokuapp.com/)