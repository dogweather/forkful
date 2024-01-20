---
title:                "테스트 작성하기"
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
테스트 코딩은 코드가 의도한 대로 작동하는지 확인하기 위해 사전에 작성된 시나리오를 실행하는 과정입니다. 프로그래머들은 버그 예방, 기능 안정성 확보, 그리고 미래의 코드 변경에 대한 자신감을 갖기 위해 테스트를 작성합니다.

## How to: (어떻게 하나요?)
```elixir
# test/example_test.exs
defmodule ExampleTest do
  use ExUnit.Case
  doctest Example

  test "더하기 기능 체크" do
    assert 2 + 3 == 5
  end
end
```
실행 결과:
```elixir
...

Finished in 0.03 seconds
1 test, 0 failures

Randomized with seed 918032
```

## Deep Dive (심화 학습)
엘릭서는 ExUnit이라는 테스트 도구를 기본 제공합니다. ExUnit은 Elixir 1.0 이후부터 함께 제공되었고, 테스트 중심 개발(Test-Driven Development - TDD)을 지원하도록 설계되었습니다. 대안으로는 ESpec(유닛 테스트를 위한 BDD 도구) 등이 있으나 ExUnit이 가장 널리 사용됩니다. ExUnit을 사용할 때는, 각 테스트 케이스마다 독립된 환경에서 실행되어 서로 간섭되지 않는다는 점을 이해해야 합니다.

## See Also (관련 정보)
- [Elixir의 공식 웹사이트](https://elixir-lang.org/)
- [ExUnit 공식 문서](https://hexdocs.pm/ex_unit/ExUnit.html)
- [ESpec GitHub 페이지](https://github.com/antonmi/espec)