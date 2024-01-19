---
title:                "랜덤 숫자 생성하기"
html_title:           "Rust: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Rust에서 랜덤 수 생성하기 

## 무엇이며 왜합니까?
랜덤 수 생성은 예측할 수 없는 수를 알고리즘 또는 하드웨어 장치를 통해 만드는 것입니다. 이는 게임, 보안, 시뮬레이션 등 다양한 분야에서 중요한 역할을 합니다.

## 어떻게 하는지
Rust에서 랜덤 수를 생성하는 가장 간단한 방법은 'rand' 크레이트를 사용하는 것입니다. 'rand'는 Rust 라이브러리에 포함된 표준 랜덤 수 생성기입니다.

```Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    println!("랜덤 수: {}", rng.gen::<u32>());
}
```
위 코드를 실행하면 의도하지 않은 u32 수를 출력합니다. 각 실행마다 다른 값이 출력될 수 있습니다.

## 깊게 알아보기
랜덤 수 생성기는 암호학, 카지노 게임, 컴퓨터 시뮬레이션 등에서 오랜 시간 동안 사용되어 왔습니다. 빠르고 예측할 수 없는 랜덤 수를 생성하는 데는 많은 향상이 이루어졌습니다. Rust의 'rand' 크레이트는 이러한 향상을 반영하고 있습니다.

'rand' 대안으로는 'getrandom' 또는 'fastrand' 같은 크레이트가 있습니다. 이들은 각각 OS 기반의 랜덤 수 생성 및 빠른 (하지만 약간 덜 안전한) 랜덤 수 생성을 제공합니다.

'rand' 크레이트의 랜덤 수 생성기는 내부적으로 'ChaCha20' 암호 스트림을 사용합니다. 이 알고리즘은 20라운드의 암호화를 사용하여 랜덤성을 보장합니다.

## 참고 자료
- [The Rust Programming Language Book - rand](https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html)
- [Rust 'rand' crate documentation](https://docs.rs/rand/0.8.3/rand/)
- [Rust 'getrandom' crate](https://docs.rs/getrandom/0.2.3/getrandom/)
- [Rust 'fastrand' crate](https://docs.rs/fastrand/1.4.0/fastrand/)