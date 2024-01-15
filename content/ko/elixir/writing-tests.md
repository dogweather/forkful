---
title:                "프로그래밍 테스트 작성"
html_title:           "Elixir: 프로그래밍 테스트 작성"
simple_title:         "프로그래밍 테스트 작성"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## 왜?

테스트를 작성하는 이유는 코드를 검증하고 버그를 찾는 것입니다. 테스트는 코드의 신뢰성을 높여주며 개발과 유지 보수를 쉽게 만들어줍니다.

## 방법

우리는 Elixir 언어를 사용하여 간단한 덧셈 함수를 작성하고 이를 테스트해보겠습니다. 먼저 `test` 모듈을 불러와야 합니다.

```Elixir
import ExUnit.Case
```

이제 덧셈 함수를 작성해봅시다. 두 수를 더한 결과를 리턴하는 함수입니다.

```Elixir
def add(x, y) do
  x + y
end
```

이제 `test` 모듈 안에 테스트 케이스를 작성합니다. `assert`를 사용하여 함수가 예상한 결과를 리턴하는지 확인합니다.

```Elixir
test "adding numbers" do
  assert add(3, 5) == 8
end
```

이제 터미널에서 `mix test`를 실행하여 테스트를 수행해봅시다. 결과는 아래와 같이 나올 것입니다.

```Elixir
Compiling 1 file (.ex)
..
Finished in 0.02 seconds
1 test, 0 failures
```

테스트를 통과했습니다! 이제 다른 케이스를 추가해서 더 많은 테스트를 작성해보세요.

## 깊이 들어가기

테스트 코드는 `assert` 대신에 `refute`를 사용하여 예상치 못한 결과를 확인할 수도 있습니다. 또한 `setup`과 `teardown` 함수를 사용하여 테스트 실행 전후에 수행되는 작업을 할 수도 있습니다.

더 자세한 내용은 Elixir 공식 문서인 [ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)을 참고해보세요.

## 연관 항목

- [Elixir 공식 홈페이지](https://elixir-lang.org/)
- [TDD (Test-Driven Development) 소개 영상](https://www.youtube.com/watch?v=2W1RZflJchQ)
- [Erlang 언어와의 차이점](https://elixirschool.com/ko/lessons/basics/keywords-and-maps/#11)