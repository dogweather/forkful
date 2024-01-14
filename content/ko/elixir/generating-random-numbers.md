---
title:    "Elixir: 랜덤 숫자 생성하기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

랜덤 숫자를 생성하는 것이 중요한 이유는 다양한 응용 분야에서 임의의 값이 필요하기 때문입니다. 예를 들어 게임에서 적들의 위치나 적절한 보상을 계산하기 위해 랜덤 숫자를 사용합니다.

## 방법

랜덤 숫자를 생성하는 방법은 간단합니다. 먼저 Elixir의 내장 함수인 `:rand.uniform/0`을 사용하여 0과 1 사이의 임의의 실수를 생성할 수 있습니다. 다음 예시 코드를 살펴봅시다.

```Elixir
number = :rand.uniform()
IO.puts("The random number is #{number}")
```

위 코드를 실행하면 화면에 "The random number is 0.783"와 같이 랜덤 실수가 출력됩니다.

또한, 임의의 정수를 생성하기 위해서는 `:rand.uniform/1` 함수를 사용할 수 있습니다. 이 함수는 매개변수로 최솟값과 최댓값 사이의 임의의 정수를 생성합니다. 예시 코드를 살펴봅시다.

```Elixir
number = :rand.uniform(1, 100)
IO.puts("The random number is #{number}")
```

위 코드를 실행하면 화면에 "The random number is 47"와 같이 1부터 100 사이의 임의의 정수가 출력됩니다.

## 깊이 파고들기

Elixir는 `:rand` 모듈을 통해 랜덤 숫자를 생성할 수 있습니다. 이 모듈은 `seed/0` 함수를 제공하는데, 이를 사용하면 랜덤 숫자의 시드 값을 설정할 수 있습니다. 시드 값은 랜덤 숫자를 생성하는 알고리즘에 영향을 주는 값으로, 동일한 시드 값을 설정하면 항상 같은 랜덤 숫자가 생성됩니다.

이외에도 Elixir에서는 더 강력한 랜덤 숫자 생성 라이브러리인 `:rand_compat`과 `:random`도 제공됩니다. 이 라이브러리를 사용하면 다양한 분포의 랜덤 숫자를 생성할 수 있으며, 추가적인 기능들도 제공합니다.

## 더보기

- [Elixir 공식 문서 - :rand 모듈](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#rand/1)
- [Elixir 공식 문서 - :rand_compat 모듈](https://hexdocs.pm/rand_compat/readme.html)
- [Elixir 공식 문서 - :random 모듈](https://hexdocs.pm/elixir/Random.html)