---
title:                "난수 생성"
date:                  2024-01-27T20:33:59.191898-07:00
model:                 gpt-4-0125-preview
simple_title:         "난수 생성"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?

프로그래밍에서 난수 생성은 시뮬레이션, 테스팅, 암호화, 게임을 만드는 데 필수적일 수 있습니다. Gleam에서는 개발자가 응용 프로그램에서 예측 불가능성을 도입하거나 실세계 시나리오를 시뮬레이션할 수 있게 해주는 기능입니다.

## 방법:

Gleam에서 난수를 생성하기 위해 주로 `gleam_random` 라이브러리를 사용합니다. 이 라이브러리는 난수 정수, 부동 소수점 및 기타 여러 기능을 생성하기 위한 함수를 제공합니다. 먼저 의존성으로 `gleam_random`을 `rebar.config` 또는 `mix.exs` 파일에 추가했는지 확인하세요.

몇 가지 예시로 들어가 보겠습니다:

### 임의의 정수 생성

지정된 범위 내에서 임의의 정수를 생성하려면 `int` 함수를 사용할 수 있습니다:

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

이 함수는 1과 10 사이의 임의의 정수를 생성합니다(포함).

### 임의의 부동 소수점 생성

임의의 부동 소수점을 얻으려면 `float` 함수를 사용하세요. 이 함수는 0.0과 1.0 사이의 부동 소수점을 생성합니다:

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### 예시 출력

이러한 함수를 실행하면 다음과 같은 출력을 볼 수 있습니다:

- `generate_random_int()`의 경우: `5`
- `generate_random_float()`의 경우: `0.84372`

난수의 특성 때문에 매 실행마다 다른 출력이 나올 수 있음을 기억하세요.

## 깊은 탐구

`gleam_random` 모듈은 의사랜덤번호생성기(PRNG)를 구현하는데, 이는 생성된 숫자가 진정으로 무작위가 아니지만 예측하기 어려워 무작위성을 모방함을 의미합니다. PRNG는 시드로 알려진 초기 값으로 시작하여 수열을 생성하기 위해 수학적 연산을 적용하는 방식으로 동작합니다.

역사적으로, 언어와 라이브러리는 메르센 트위스터 또는 선형 합동 생성기(LCG)와 같은 여러 알고리즘을 PRNG에 구현했습니다. 알고리즘 선택은 "무작위성"의 품질에 영향을 주며, 일부는 다른 것보다 암호화 응용 프로그램에 더 적합합니다. Gleam의 표준 라이브러리는 `gleam_random` 모듈로 편리함과 사용의 쉬움을 제공하지만, 암호학적으로 안전한 난수가 필요한 경우 항상 최선의 선택이 아닐 수 있습니다. 암호화 목적의 경우, 생성된 숫자의 시퀀스를 관찰함으로써 미래의 숫자를 예측할 수 있는 공격에 대해 견뎌낼 수 있도록 설계된 암호학적으로 안전한 의사랜덤번호생성기(CSPRNGs)를 제공하는 라이브러리를 알아보아야 합니다.

결론적으로, Gleam의 난수 생성 기능은 일반 프로그래밍 요구 사항에 강력하지만, 특정 보안 요구 사항이 있는 애플리케이션은 난수 생성의 무결성 및 보안을 보장하기 위해 전용 암호화 솔루션을 고려해야 합니다.
