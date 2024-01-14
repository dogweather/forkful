---
title:    "Rust: 랜덤 숫자 생성"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 왜

랜덤한 숫자를 생성하는 것에 관심을 가지는 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 게임 개발에서 적절한 어려움을 조절하거나, 보안 암호 생성 등에 사용할 수 있습니다.

## 방법

Rust는 기본적으로 랜덤한 숫자를 생성할 수 있는 기능을 제공합니다. 예를 들어, 1부터 100 사이의 랜덤한 숫자를 생성하는 방법은 다음과 같습니다.

```Rust
use rand::Rng;

fn main() {
  let mut rng = rand::thread_rng();
  let random_number: u32 = rng.gen_range(1, 101);

  println!("{}", random_number);
}
```

위 코드에서는 rand 모듈을 사용하고, thread_rng() 함수를 통해 랜덤한 숫자를 생성하는 데 필요한 발생기를 만듭니다. 그리고 gen_range() 함수를 통해 원하는 범위 내에서 숫자를 생성합니다. 마지막으로 생성된 숫자를 출력합니다.

## 심층 분석

Rust에서는 기본적으로 pseudo-random number generator (PRNG)를 사용하여 랜덤한 숫자를 생성합니다. 이 PRNG는 seed 값에 따라서 다양한 변형된 숫자를 생성합니다. Rust의 PRNG는 Mersenne Twister 알고리즘을 기반으로 하며, 안전한 seed 값을 생성하기 위해 OS의 여러 가지 source를 조합하여 사용합니다.

## 관련 자료

- [Rust 공식 문서 - 난수 생성](https://doc.rust-lang.org/std/rand/index.html)
- [Thread-Local PRNG: rand::thread_rng()](https://docs.rs/rand/0.3.16/rand/fn.thread_rng.html)
- [Random Range Generation: gen_range()](https://doc.rust-lang.org/std/rand/trait.Rng.html#method.gen_range)
- [Mersenne Twister Algorithm](https://en.wikipedia.org/wiki/Mersenne_Twister)