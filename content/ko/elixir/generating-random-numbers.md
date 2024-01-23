---
title:                "난수 생성하기"
date:                  2024-01-20T17:48:52.407323-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 및 왜?)
랜덤 숫자 생성은 예측할 수 없는 숫자를 만드는 것입니다. 프로그래머들은 게임, 시뮬레이션, 보안 등 다양한 분야에서 랜덤성을 필요로 합니다.

## How to: (어떻게 하나요?)
Elixir에서 랜덤 숫자를 생성하려면, `:rand` 모듈을 사용하세요. 간단하죠.

```elixir
# 시드를 초기화합니다 - 일반적으로 이것은 한 번만 필요합니다
:rand.seed(:exsplus)

# 0에서 10까지의 랜덤 정수를 생성합니다
random_integer = :rand.uniform(11)
IO.puts(random_integer)

# 1에서 6까지의 랜덤 정수를 생성합니다 - 주사위를 생각하세요
dice_roll = :rand.uniform(6)
IO.puts(dice_roll)
```
출력은 실행할 때마다 달라집니다, 예를 들면:
```
7
3
```

## Deep Dive (심층 분석)
랜덤 숫자 생성은 컴퓨터 과학의 오랜 문제입니다. 컴퓨터는 엄밀한 규칙을 따르기 때문에 진정한 무작위성을 만들기 어렵습니다. Elixir는 Erlang의 `:rand` 모듈을 사용하여 난수를 생성합니다. 이는 Psuedo-random Number Generators (PRNGs)를 사용합니다. 시드가 변경될 때마다 결과 값들의 시퀀스도 변합니다. Elixir/Erlang에서 `:exsplus` 알고리즘은 고품질의 난수를 생성하는 기본 옵션입니다.

다른 방법도 있습니다. 예를 들어, 암호화에 사용되는 더 안전한 난수 생성기를 사용할 수도 있습니다. 그러나 대부분의 일반적인 사례에는 `:rand` 모듈로 충분합니다.

## See Also (더 보기)
- [Elixir `:rand` documentation](https://erlang.org/doc/man/rand.html)
- [PRNG in Cryptography on Wikipedia](https://en.wikipedia.org/wiki/Pseudorandom_number_generator#In_cryptography)
