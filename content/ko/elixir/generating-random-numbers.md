---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

랜덤 숫자 생성이란, 프로그램이 예측할 수 없는 숫자를 만드는 것입니다. 이는 게임, 암호화, 시뮬레이션 등 다양한 분야에서 특별한 동작을 수행하거나, 무작위 결과를 생성하는데 사용됩니다. 

## 방법:

랜덤 숫자를 생성하려면 Elixir의  `:rand.uniform/1` 함수를 사용할 수 있습니다.

```Elixir
num = :rand.uniform(100)
IO.puts(num)
```
이 코드는 0이상 100 미만의 랜덤 숫자를 출력합니다.

## 깊이 들어가서 보기:

랜덤 숫자 생성기는 컴퓨터 과학의 중요한 부분으로 오랜 시간 동안 다루어져 왔습니다. 가장 간단한 랜덤 숫자 생성기는 선형 동등 생성기로, 이는 단순히 이전 숫자에 대한 연산의 결과를 제공합니다.

하지만 이런 방식은 시간이 지남에 따라 예측 가능해집니다. 따라서 복잡한 알고리즘이 사용되곤 합니다. Elixir의 :rand 모듈은 이러한 복잡한 알고리즘 중 하나를 사용합니다. 

랜덤 숫자를 생성하는 다른 방법으로는 변동 비트를 사용하는 방법이 있습니다. 변동 비트는 하드웨어에서 발생하는 무작위성에 의존합니다.

## 추가로 보기:

- Elixir :rand 모듈 문서: https://hexdocs.pm/elixir/1.12/Kernel.html#rand/0 
- 랜덤 숫자 생성에 대한 상세한 설명: https://en.wikipedia.org/wiki/Random_number_generation
- 변동 비트 활용방법: https://www.cs.berkeley.edu/~daw/papers/ddj-netscape.html