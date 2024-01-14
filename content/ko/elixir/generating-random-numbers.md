---
title:                "Elixir: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜

랜덤 숫자를 생성하는 것이 왜 중요한지 궁금하신가요? 보통 암호화나 시뮬레이션 등 다양한 프로그래밍 분야에서 랜덤 숫자가 필요합니다.

# 사용 방법

랜덤 숫자를 생성하는 가장 간단한 방법은 Elixir의 `:random` 모듈을 사용하는 것입니다. 아래는 0부터 100까지의 랜덤 숫자를 생성하는 코드 예시입니다.

```Elixir
random_number = :random.uniform(0, 100)
IO.puts(random_number)
```
출력 예시:
` 53 `

또 다른 방법으로는 `Enum.random/1` 함수를 사용하는 것입니다. 이 함수에는 리스트 혹은 범위를 인자로 전달할 수 있습니다. 아래는 리스트에서 랜덤하게 요소를 선택하는 코드 예시입니다.

```Elixir
random_element = Enum.random(["apple", "orange", "banana", "grape"])
IO.puts(random_element)
```
출력 예시:
`"orange"`

# 깊이 들어가기

랜덤 숫자를 생성할 때 중요한 것은 사실적인 결과뿐만 아니라 재현성(reproducibility)도 중요합니다. 다시 실행해도 같은 결과를 얻을 수 있어야 합니다. 따라서, 이를 위해 Elixir에서는 `:random.seed/1` 함수를 사용합니다. 이 함수에는 랜덤 숫자를 생성할 때 사용할 시드(seed) 값을 전달합니다. 시드 값을 미리 설정해 놓으면 같은 시드 값을 사용할 때마다 같은 결과를 얻을 수 있습니다.

또한, 랜덤 숫자를 생성하기 위해 가장 널리 사용되는 알고리즘은 선형 합동법(linear congruential generator)입니다. 이 알고리즘은 현재 시간을 시드 값으로 사용하며, 빠르고 간단하지만 예측 가능성이 높은 단점이 있습니다. 따라서 보안이 중요한 경우에는 더 안전한 랜덤 숫자 생성 방법을 사용해야 합니다.

# 참고자료

[Learn Elixir: generating random numbers](https://bigmachine.io/products/the-little-elixir-otp-guidebook/chapters/random-numbers)

[Using Random Number Generators in Elixir](https://pusher.com/tutorials/pseudo-random-number-generators-elixir)

See Also:

[Enum 모듈 문서](https://hexdocs.pm/elixir/Enum.html)

[Random 모듈 문서](https://hexdocs.pm/elixir/Random.html)