---
title:                "Elixir: 테스트 작성하기"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-tests.md"
---

{{< edit_this_page >}}

Elixir 프로그래밍을 한국어로 배우고 있다면 아마도 이미 Elixir 언어가 얼마나 강력하고 효율적인지 알고 계실 것입니다. 하지만 저는 Elixir 프로그래밍 새로운 면을 발견했습니다 - 소프트웨어 테스팅! 이 글에서는 Elixir 프로그래밍에 테스트가 어떻게 도움이 되는지, 그리고 어떻게 테스트를 작성할 수 있는지 자세히 알아보겠습니다.

## 왜 테스트를 작성해야 할까요?

프로그래밍은 항상 예기치 않은 상황들이 발생합니다. 그리고 이러한 상황에서 우리는 작성한 코드가 제대로 동작하는지 확인해야 합니다. 이때 테스트를 작성하면 이러한 상황들을 더 쉽게 다룰 수 있습니다. 또한 테스트를 작성하면 코드의 안정성과 신뢰성을 높일 수 있습니다. 이제 어떻게 테스트를 작성할 수 있는지 살펴보겠습니다.

## 방법은 어떻게 되나요?

Elixir에서 테스트를 작성하는 기본적인 방법은 `test/0` 함수를 이용하는 것입니다. 다른 함수들과 마찬가지로 `def` 키워드를 사용하여 선언할 수 있습니다.

```Elixir
def test "addition" do
  assert Calculator.add(2, 2) == 4
end
```

위 예제에서는 `Calculator` 모듈의 `add/2` 함수를 테스트하는 예제입니다. `assert` 함수를 사용하여 함수의 반환값이 예상한 값과 일치하는지 확인합니다. 다른 예제를 살펴보겠습니다.

```Elixir
def test "removing nil elements" do
  input = [1, 2, nil, 4]
  output = List.delete(input, nil)
  assert output == [1, 2, 4]
end
```

위 예제는 리스트에서 `nil` 값을 제거하는 `List.delete/2` 함수를 테스트하는 예제입니다. 매번 새로운 값이 나오는지 확인하여 테스트의 일관성을 유지합니다. 이제 테스트 작성에 대해 더 깊이 알아보겠습니다.

## 깊이 파고들기

Elixir에서 테스트를 작성하는 또 다른 방법은 `doctest/2` 함수를 사용하는 것입니다. `doctest/2` 함수는 고유한 함수 이름과 함수를 작성한 라인의 주석을 인자로 받습니다. 다음 예제를 살펴보겠습니다.

```Elixir
  @doc """
  Adds two numbers together.

  ## Examples

      iex> Calculator.add(2, 2)
      4
  """
  def add(a, b) do
    a + b
  end
```

위 예제에서는 `add/2` 함수를 작성하면서 바로 밑에 `@doc` 주석과 `doctest/2` 함수를 사용하여 예제를 넣어주었습니다. 이렇게 작성한 예제는 `iex` 콘솔에서 자동으로 테스트해볼 수 있습니다. 또 다른 예제를 살펴보겠습니다.

```Elixir
  @doc """
  Removes nil elements from a list.

  ## Examples

      iex> List.delete([1, 2, nil, 4], nil)
      [1, 2, 4]
  """
  def delete(list, element) do
    Enum.filter(list, &(&1 != element))
  end
```

위 예제에서는 `delete/2` 함수를 작성하면서 예제를 넣어줄 때 `iex`