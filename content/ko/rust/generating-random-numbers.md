---
title:                "난수 생성하기"
date:                  2024-01-20T17:50:16.920793-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
랜덤 숫자 생성이란 예측할 수 없는 숫자를 만드는 것입니다. 프로그래머들은 게임, 시뮬레이션, 보안 등 다양한 분야에서 예측 불가능한 결과나 데이터를 생성하기 위해 사용합니다.

## How to: (방법)
Rust에서의 랜덤 숫자 생성은 `rand` 크레이트를 사용합니다. 아래 예제에서는 어떻게 랜덤 숫자를 생성하는지 보여줍니다:

```Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let random_number: u8 = rng.gen_range(0..100);
    println!("랜덤 숫자: {}", random_number);
}
```
출력 예시:
```
랜덤 숫자: 42
```

`gen_range`를 이용해 0부터 99까지의 랜덤 숫자를 만들 수 있습니다.

## Deep Dive (심층 탐구)
랜덤 숫자 생성은 컴퓨터 과학 초기부터 중요했습니다. 진정한 '랜덤'은 기계에서 어렵기 때문에 Pseudo-Random Number Generators (PRNGs)를 사용합니다. 이는 결정적인 알고리즘을 통해 랜덤처럼 보이는 수열을 생성합니다.

`rand` 크레이트는 여러 PRNG 알고리즘들을 제공하며 성능과 보안 중 어떤 것을 중시할지에 따라 선택할 수 있습니다. 예를 들어, `thread_rng`는 현재 스레드에 대한 높은 성능의 PRNG를 제공합니다.

Rust에서는 `rand` 외에도 `fastrand`나 `oorandom`과 같은 다른 크레이트들을 이용할 수 있습니다. 각각은 장단점이 있으니 상황에 맞게 선택하세요.

## See Also (더 보기)
- Rust `rand` crate documentation: https://docs.rs/rand/
- Rust by Example - Random: https://doc.rust-lang.org/rust-by-example/std_misc/rand.html
- Wikipedia on Pseudo-Random Number Generators: https://en.wikipedia.org/wiki/Pseudorandom_number_generator

이러한 자료들은 랜덤 숫자 생성에 대한 더 심도 있는 이해와 다른 알고리즘, 크레이트들에 대한 정보를 제공합니다.