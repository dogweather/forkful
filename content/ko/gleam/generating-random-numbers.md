---
title:                "Gleam: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

난수 생성에 참여하는 이유는 무엇일까요? 난수는 컴퓨터 프로그램에서 매우 중요합니다. 게임에서 적, 카드 게임에서 카드 순서, 테스트 데이터 생성 등 다양한 용도로 사용될 수 있습니다. 따라서 난수를 만드는 것은 컴퓨터 프로그래밍에서 필수적인 요소입니다.

## 만드는 방법

Gleam은 다양한 방법으로 난수를 생성할 수 있습니다. 가장 간단한 방법은 `random.float` 함수를 사용하는 것입니다. 이 함수는 0과 1 사이의 난수를 생성합니다.

```Gleam
import gleam/random
import gleam/string

let random_number = random.float()
string.print("Random number: ", random_number)
```

출력 예시:

```
Random number: 0.4892796162425797
```

더 복잡한 난수를 생성하고 싶다면 `random.int` 함수를 사용할 수 있습니다. 이 함수는 인자로 최솟값과 최댓값을 받아 해당 범위 내의 난수를 생성합니다.

```Gleam
import gleam/random
import gleam/string

let random_number = random.int(1, 10)
string.print("Random number between 1 and 10: ", random_number)
```

출력 예시:

```
Random number between 1 and 10: 7
```

## 깊이 파헤치기

Gleam에서 난수를 만드는 방법은 매우 다양합니다. `random.float` 함수는 실제로는 `random.int` 함수를 활용하여 구현됩니다. `random.int` 함수에 인자로 0과 최댓값 사이의 정수를 주면 그 값에 1을 더한 후 최종적으로 0부터 그 값 사이의 난수를 생성합니다. 이를 활용하면 `random.float` 함수를 직접 구현해볼 수 있습니다.

```Gleam
import gleam/random
import gleam/math

fn float() -> Float {
  let max = 1.0
  let int = random.int(0, math.f64_to_int(max))
  math.int_to_f64(int) / max
}
```

## 부가 정보

- [Gleam 공식 문서: 난수 생성하기](https://gleam.run/documentation/stdlib/random#float)
- [Gleam 공식 문서: 수학 함수 사용하기](https://gleam.run/documentation/stdlib/math#int_to_f64)

## 더 알아보기

위에서 소개한 `random.float`와 `random.int` 함수는 예측 가능한 난수를 생성합니다. 만약 완벽하게 무작위성을 보장하는 난수가 필요하다면 더 복잡한 방식의 난수 생성 알고리즘을 사용해야 합니다. 예를 들어, Linear Congruential Generator (LCG)나 Mersenne Twister 알고리즘을 사용할 수 있습니다. 하지만 이 알고리즘들은 구현하기가 더 까다롭기 때문에 간단하고 예측 가능한 `random.float` 함수를 사용하는 것이 좋습니다.