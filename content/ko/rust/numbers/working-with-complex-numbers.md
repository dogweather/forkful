---
date: 2024-01-26 04:45:41.397913-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Rust\uB294 \uB0B4\uC7A5\uB41C \uBCF5\uC18C\
  \uC218 \uC9C0\uC6D0\uC774 \uC5C6\uC9C0\uB9CC, `num-complex`\uC640 \uAC19\uC740 \uD06C\
  \uB808\uC774\uD2B8\uAC00 \uC788\uC2B5\uB2C8\uB2E4. \uC0AC\uC6A9 \uBC29\uBC95\uC740\
  \ \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.905825-06:00'
model: gpt-4-0125-preview
summary: "Rust\uB294 \uB0B4\uC7A5\uB41C \uBCF5\uC18C\uC218 \uC9C0\uC6D0\uC774 \uC5C6\
  \uC9C0\uB9CC, `num-complex`\uC640 \uAC19\uC740 \uD06C\uB808\uC774\uD2B8\uAC00 \uC788\
  \uC2B5\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
weight: 14
---

## 사용 방법:
Rust는 내장된 복소수 지원이 없지만, `num-complex`와 같은 크레이트가 있습니다. 사용 방법은 다음과 같습니다:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let sum = a + b;
    let product = a * b;

    println!("합: {}", sum); // 합: 3 - 1i
    println!("곱: {}", product); // 곱: 14 - 5i
}
```
이 마법을 일으키기 위해서는 `Cargo.toml`에 `num_complex`를 추가해야 합니다.

## 심층적 이해
복소수는 16세기에 처음 등장했지만, 오일러와 같은 수학자들이 이것으로 노는 18세기에 정말 발전하기 시작했습니다.

내장 복소수 연산이 없는 언어들은 Rust처럼 제3자 라이브러리에 의존합니다. `num-complex`는 그러한 크레이트 중 하나이며, Rust의 숫자형 타입과 특성을 제공하려는 `num` 크레이트 컬렉션의 일부입니다.

어떤 언어들(예: Python)은 복소수에 대한 내장 지원을 가지고 있는 반면, 다른 언어들(예: `<complex>` 헤더를 가진 C++)은 표준 라이브러리의 일부로 제공합니다. Rust에서는 표준 라이브러리를 작게 유지하기로 한 결정으로 인해 종종 커뮤니티가 만든 크레이트를 추가 기능성을 위해 찾게 됩니다.

## 또한 보기
- [Rust 책](https://doc.rust-lang.org/book/): Rust와 외부 크레이트를 다루는 방법에 대해 더 배우기 위해.
- [복소수 위키백과](https://en.wikipedia.org/wiki/Complex_number): 복소수에 대한 더 깊은 이해를 위해.
