---
title:                "Elixir: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-tests.md"
---

{{< edit_this_page >}}

"## 왜"
에릭서 프로그래밍을 할 때 테스트를 작성하는 이유는 무엇인가요? 1-2 문장으로 요약해주세요.

테스트를 작성하는 이유는 우리의 코드를 검증하고 오류를 최소화하기 위해서입니다. 테스트는 코드의 안정성을 높이고 유지보수를 더 쉽게 만들어줍니다.

"## 작성 방법"
코드 블록에 "```Elixir ... ```" 코드 예제와 출력 코드를 포함해주세요.

 ```
defmodule Calculator do
  # 두 수를 더하는 간단한 함수 예제
  def add(x, y) do
    x + y
  end
end
 ```
```
# 출력 코드:
iex> Calculator.add(3, 5)
8
```

또한, 각 함수에 대한 테스트를 작성하는 것도 중요합니다. "ExUnit" 라이브러리는 이 과정을 더욱 쉽게 만들어줍니다.

 ```
defmodule CalculatorTest do
  use ExUnit.Case
  
  # 덧셈 함수가 정상적으로 작동하는지 테스트
  test "addition" do
    assert Calculator.add(3, 5) == 8
  end
end
 ```
```
# 출력 코드:
iex> mix test
.
1 test, 0 failures
```

"## 깊게 파헤치기"
테스트 작성에 대한 더욱 깊은 정보는 어떤 것이 있을까요? 

테스트는 우리의 코드를 검증하는 것뿐만 아니라, 코드 작성과 디자인에도 영향을 미칩니다. 테스트를 작성하면서 우리는 더욱 견고한 코드를 작성하게 됩니다. 또한, 테스트는 버그를 찾고 수정하는데 도움이 되기도 합니다.

"## 참고 자료"
"See Also" 라는 제목으로 마무리하고, 관련된 링크를 나열해주세요.

- Elixir 문서: https://elixir-lang.org/docs.html
- ExUnit 라이브러리: https://hexdocs.pm/ex_unit/ExUnit.html 
- 테스트에 대한 더 많은 정보: https://elixirforum.com/t/how-to-write-a-test-or-why-you-should-always-write-tests/323