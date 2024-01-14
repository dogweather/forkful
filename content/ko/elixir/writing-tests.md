---
title:    "Elixir: 프로그래밍에서의 테스트 작성"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-tests.md"
---

{{< edit_this_page >}}

# 왜

테스트 코드를 작성하는 이유는 우리 코드를 더욱 견고하고 신뢰성있게 만들어주기 때문입니다. 우리는 개발을 하면서 발생할 수 있는 버그나 문제를 미리 발견하고 수정할 수 있도록 테스트 코드를 작성해야 합니다.

## 작성하는 법

테스트 코드를 작성하는 방법은 아주 간단합니다. 가장 먼저 `ExUnit` 라이브러리를 사용해야합니다. `ExUnit` 은 별도의 설치 없이 기본적으로 포함되어 있으며 `ExUnit.Case` 모듈을 사용하면 바로 테스트 코드를 작성할 수 있습니다.

예제 코드를 통해 살펴보겠습니다. 우리는 간단한 함수인 `factorial(n)`이 입력 된 수의 팩토리얼 값을 계산하는 함수를 작성하려고 합니다. 먼저 `test_factorial` 이라는 이름의 테스트 함수를 작성하고 그 내부에 `assert` 문을 사용하여 기대하는 결과와 실제 결과를 비교합니다.

```elixir
defmodule FactorialTest do
  use ExUnit.Case

  test "factorial of 1 is 1" do
    assert factorial(1) == 1
  end

  test "factorial of 5 is 120" do
    assert factorial(5) == 120
  end
end
```

위 예제에서 `factorial/1` 함수는 아직 작성되지 않았으므로 에러가 발생할 것입니다. 이제 해당 함수를 작성하고 `ExUnit` 라이브러리로 테스트를 실행해보면 테스트가 모두 성공한다는 것을 확인할 수 있습니다.

## 깊이 파고들기

테스트 코드를 작성하는 방법은 다양한 기술과 패턴이 존재합니다. 우리는 `setup` 을 통해 테스트 실행 전에 필요한 환경을 설정하고, `teardown` 을 통해 테스트 실행 후에 환경을 정리하는 등 유용한 기능들을 사용할 수 있습니다.

또한 `Case` 모듈의 다양한 함수들을 통해 특정 조건에서 테스트를 실행하거나 특정 예외가 발생하는지 테스트할 수 있습니다.

테스트 코드를 작성하는 방법에 대해 더 자세히 알고 싶다면 공식 문서나 다른 블로그 포스트를 참고하는 것도 좋은 방법입니다.

# See Also

- [공식 Elixir ExUnit 문서](https://hexdocs.pm/ex_unit/ExUnit.html)
- [좋은 테스트 코드 작성 방법 (영어)](https://dev.to/mattthebaker/writing-good-tests-in-elixir-2ja8)