---
title:                "디버그 출력하기"
html_title:           "Elixir: 디버그 출력하기"
simple_title:         "디버그 출력하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 기록하는 것이 왜 중요한지 궁금하신가요? 좋은 질문이에요! 디버깅은 소프트웨어 개발 과정에서 가장 중요한 단계 중 하나입니다. 디버그 출력은 코드가 어떤식으로 실행되는지를 추적하고 버그를 찾는데 도움을 주며, 문제가 발생하는 부분을 이해하는데 큰 도움이 됩니다.

## 어떻게

디버그 출력을 기록하는 방법은 간단합니다. 우선 디버그를 원하는 줄에 `IO.inspect` 함수를 호출합니다. 이 함수는 원하는 값 또는 변수를 인자로 받습니다. 예를 들어, 만약 변수 `number`의 값을 확인하고 싶다면 `IO.inspect(number)`와 같이 작성하면 됩니다.

```Elixir
number = 42
IO.inspect(number)

# 출력:
# 42
```

만약 여러 변수의 값을 확인하고 싶다면 `IO.inspect` 함수를 여러번 호출하면 됩니다.

```Elixir
number = 42
name = "Elixir"
IO.inspect(number)
IO.inspect(name)

# 출력:
# 42
# "Elixir"
```

디버그 출력을 포함하는 코드 블록 안에서도 사용할 수 있습니다.

```Elixir
IO.inspect("This is debug output")

# 출력:
# "This is debug output"
```

## 딥다이브

디버그 출력은 `IO.puts` 함수와 달리 실행 순서를 변경하지 않습니다. 따라서 디버그 출력 코드를 포함하여도 코드의 동작이 변하지 않습니다. 그리고 `IO.inspect` 함수는 체인 형식으로 사용할 수 있습니다. 이는 여러 변수를 한번에 확인할 수 있게 해줍니다.

```Elixir
number = 42
IO.inspect(number)
      |> IO.inspect(:atom)
      |> IO.inspect("Elixir")

# 출력:
# 42
# :atom
# "Elixir"
```

또한, `IO.inspect` 함수는 두 개의 매개변수를 받을 수도 있습니다. 첫번째 값은 일반적으로 변수이며, 두번째 값은 디버그 출력에 대한 옵션입니다. 예를 들어, `IO.inspect(number, label: "Number:")`와 같이 사용하면 디버그 출력의 앞에 "Number:"라는 레이블이 추가됩니다. 이 옵션은 변수의 이름이나 의미를 명확하게 표시하고 싶을때 유용합니다.

## 참고

- [Elixir 공식 문서](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/errors-and-exceptions#debugging-and-tracing)
- [Codecademy](https://www.codecademy.com/learn/learn-elixir/modules/learn-elixir-introduction-to-elixir/cheatsheet)

그리고 항상 디버그 출력을 자주 사용해보고 실험해보는 것이 가장 좋은 방법입니다. 디버그 출력은 개발 과정에서 가장 소중한 도구 중 하나이며, 매우 유용하게 사용할 수 있습니다.

## 더 알아보기

- [Elixir 디버깅을 위한 디버그 출력 활용하기](https://elixirschool.com/ko/lessons/basics/debugging-with-dirty-output/)
- [Elixir 디버깅 팁과 트릭들](https://elixirjapan.org/newsletter/2016/04/15/debugging-elixir-by-jose-valim)
- [디버그 테크닉: IO.inspect 맞춤형 사용하기](https://andrealeopardi.com/posts/custom_io_inspect_in_elixir/)