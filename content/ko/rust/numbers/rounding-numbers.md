---
title:                "숫자 반올림하기"
date:                  2024-01-26T03:47:21.718073-07:00
model:                 gpt-4-0125-preview
simple_title:         "숫자 반올림하기"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/rounding-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
숫자를 반올림한다는 것은 그것들을 가장 가까운 정수 또는 특정 정밀도의 분수로 조정하는 것을 말합니다. 프로그래머들은 인간이 읽기 쉽게 값을 단순화하거나, 사양 요구 사항을 충족시키거나, 부동 소수점 연산에서 계산 오버헤드를 줄이기 위해 숫자를 반올림합니다.

## 방법:
Rust는 반올림을 쉽게 해줍니다. `f32` 또는 `f64` 유형에 대해 이러한 메소드들을 확인해보세요:

```rust
fn main() {
    let num = 2.34567;

    // 가장 가까운 정수로 반올림
    let round = num.round();
    println!("반올림: {}", round); // 반올림: 2

    // 바닥 - 주어진 숫자보다 작거나 같은 가장 큰 정수
    let floor = num.floor();
    println!("바닥: {}", floor); // 바닥: 2

    // 천장 - 주어진 숫자보다 크거나 같은 가장 작은 정수
    let ceil = num.ceil();
    println!("천장: {}", ceil); // 천장: 3

    // 절삭 - 소수점 아래 없는 정수 부분
    let trunc = num.trunc();
    println!("절삭: {}", trunc); // 절삭: 2

    // 십의 거듭제곱의 가장 가까운 배수로
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("소수점 아래 2자리로 반올림: {}", multiple_of_ten); // 소수점 아래 2자리로 반올림: 2.35
}
```

## 심층 탐구
역사적으로, 반올림은 무한 소수나 무리수를 제한된 디지털 공간에 맞추는 데 필수적이었습니다—기억 공간이 부족한 고대 컴퓨터에는 필수적이었습니다. 전산기를 생각해보되, 덜 장인적이고 더 수학적인 것을 생각해보세요.

네이티브 Rust 메소드에 대한 대안으로는 다음이 있습니다:
1. 기본적으로 반올림하는 문자열 포매팅을 위한 `format!` 매크로.
2. 더 세밀한 제어를 위한 `round` 크레이트 같은 특수한 수학 작업을 위한 외부 크레이트.

내부적으로, Rust의 반올림 연산은 IEEE 표준을 준수합니다—기술 전문 용어로는 "수학 선생님이 원하는 대로 반올림한다"는 뜻입니다. 게다가, 이진 표현 때문에 일부 숫자들은 전통적으로 반올림될 수 없습니다, 예를 들어 0.1처럼 이진수로 표현될 때 무한한 표현을 갖는 숫자들입니다.

## 참고
- Rust 기본 타입 메소드에 대한 문서: https://doc.rust-lang.org/std/primitive.f64.html
- 부동 소수점 산술에 대한 IEEE 표준 (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- 더 복잡한 반올림을 위한 "round" 크레이트: https://crates.io/crates/round
