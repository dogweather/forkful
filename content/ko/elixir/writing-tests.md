---
title:    "Elixir: 테스트 작성"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 왜

소프트웨어 개발을 진행하다 보면 자주 발생하는 문제 중 하나는 버그입니다. 이 버그들은 프로그램 실행 중에 발생하는 오류로, 그 해결을 위해 시간과 비용을 많이 소모할 수 있습니다. 이러한 문제를 해결하기 위해 엄격한 테스트를 통해 버그를 예방하고 높은 품질의 소프트웨어를 개발하도록 노력하는 것이 중요합니다.

## 방법

테스트를 작성하는 가장 기본적인 방법은 'ExUnit' 프레임워크를 사용하는 것입니다. 이 프레임워크는 Elixir에서 테스트를 작성하고 실행하고 관리하는 데 필요한 모든 기능을 제공합니다.

아래는 간단한 덧셈 함수를 테스트하는 예시입니다. ```Elixir
defmodule Calculator do
  def add(a, b) do
    a + b
  end
end
```

```Elixir
defmodule CalculatorTest do
  use ExUnit.Case

  test "덧셈 함수 테스트" do
    assert Calculator.add(2, 3) == 5
  end
end
```

테스트 실행 결과는 터미널에서 ```Elixir
mix test
```

을 입력하면 확인할 수 있습니다.

## 깊이 들어가기

테스트를 작성할 때 주의할 점이 있습니다. 첫째, 테스트는 각각 독립적이고 서로 영향을 주지 않아야 합니다. 둘째, 모든 코드를 테스트할 필요는 없지만 중요한 부분은 꼭 테스트해야 합니다. 셋째, 모든 예상되는 입력과 예상할 수 없는 입력의 조합에 대해 테스트를 작성해야 합니다.

더 자세한 내용은 [Elixir 공식 문서](https://hexdocs.pm/ex_unit/ExUnit.html)를 참고하시기 바랍니다.

## 관련 링크

- [Elixir 공식 문서](https://elixir-lang.org/getting-started/introduction.html)
- [ExUnit 공식 문서](https://hexdocs.pm/ex_unit/ExUnit.html)
- [테스트 주도 개발 (TDD)에 대한 이해](https://medium.com/@han_jjok1129/%ED%8C%8C%EC%9D%B4%EC%8D%AC-%ED%85%8C%EC%8A%A4%ED%8A%B8-%EC%A3%BC%EB%8F%84-%EA%B0%9C%EB%B0%9C-tdd-%EC%9D%B4%ED%95%B4%ED%95%98%EA%B8%B0-79552c844432)

## 더 보기 (See Also)

- [테스트 커버리지 (Coverage)에 대한 이해](https://blog.limnine.com/how-to-reduce-test-coverage-pains/)
- [문서화를 통한 테스트 케이스 작성 방법](https://blog.limnine.com/documenting-the-numb-tests-take-the-pain-away/)