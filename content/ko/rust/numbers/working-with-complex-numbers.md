---
title:                "복소수 다루기"
date:                  2024-01-26T04:45:41.397913-07:00
model:                 gpt-4-0125-preview
simple_title:         "복소수 다루기"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
복소수는 실수부와 허수부를 가지고 있으며, 엔지니어링, 물리학, 컴퓨터 그래픽스와 같은 다양한 분야에서 중요한 역할을 합니다. 프로그래머들은 일반 실수로는 해결할 수 없는 방정식을 풀기 위해 복소수를 사용합니다.

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
