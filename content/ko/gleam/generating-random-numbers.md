---
title:                "난수 생성하기"
html_title:           "Gleam: 난수 생성하기"
simple_title:         "난수 생성하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

랜덤 숫자를 생성하는 것이 왜 중요한지 궁금하셨나요? 컴퓨터 프로그래밍에서 무작위성은 매우 중요한 요소입니다. 랜덤 숫자를 생성함으로써 다양한 시나리오를 시뮬레이션하거나 보안 이슈에 대처할 수 있습니다. 게다가 재미있는 무작위 게임을 만들기도 가능합니다!

## 사용 방법

우선 Gleam 언어를 사용할 줄 알아야 합니다. 만약 아직 사용해보지 않았다면, [공식 문서](https://gleam.run/documentation/)에서 간단한 예제들을 통해 언어를 익힐 수 있습니다. 

먼저, 랜덤 숫자를 생성하는 모듈을 불러와야 합니다. 기본 모듈인 `random` 모듈을 사용해봅시다.

```Gleam
import random

let number = random.int(1, 10)
// 1부터 10 사이의 정수를 랜덤하게 생성합니다.

let float_number = random.float(0.5, 1.5)
// 0.5부터 1.5 사이의 부동 소수점 숫자를 랜덤하게 생성합니다.

let bool = random.bool()
// true 또는 false를 랜덤하게 생성합니다.
```

위의 예제들에서 볼 수 있듯이, `random` 모듈을 사용하면 다양한 형식의 랜덤 숫자를 생성할 수 있습니다. 또한, 일정한 범위 내에서 숫자를 생성하기 위해 `int`와 `float` 함수에 최솟값과 최댓값을 인자로 전달해야 합니다. 

그렇다면, 한 번에 여러 개의 랜덤 숫자를 생성하는 방법은 없을까요? 가능합니다! `random` 모듈의 `list` 함수를 사용하면 됩니다.

```Gleam
let numbers = random.list(random.int, 5, 100, 1, 10)
// 정수 5개를 생성하며, 각 숫자는 최솟값 1부터 최댓값 10까지 생성됩니다.
```

`list` 함수에서 첫 번째 인자로 전달한 `random.int`는 생성할 숫자 형식을 나타내며, 두 번째 인자는 생성할 숫자의 개수를 나타냅니다.

## 깊게 들어가보기

우리는 `random` 모듈을 사용하여 다양한 형식의 랜덤 숫자를 생성할 수 있다는 것을 확인했습니다. 그렇다면 이 모듈은 어떤 알고리즘을 사용해서 랜덤 숫자를 생성할까요?

실제로 `random` 모듈은 적게는 Mersenne Twister, 많게는 XORShift와 같은 다양한 알고리즘을 사용하여 랜덤 숫자를 생성합니다. 이는 다양한 요구에 따라 더 나은 랜덤성을 보장하기 위함입니다.

게다가, `random` 모듈의 함수들은 내부적으로 [PCG-Random 라이브러리](https://github.com/pixie-lang/pcg-random)를 사용합니다. 이 라이브러리는 매우 빠르고, 예측할 수 없는 랜덤 숫자를 생성하는데 최적화되어 있습니다.

더 깊게 들어가고 싶다면, 위에서 언급한 각 알고리즘들에 대해 더 자세히 알아보는 것을 추천합니다!

## 더 알아보기

- [Gleam