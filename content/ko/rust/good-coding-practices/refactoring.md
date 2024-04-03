---
date: 2024-01-26 03:37:03.142882-07:00
description: "\uBC29\uBC95: Rust \uCF54\uB4DC\uC758 \uAC04\uB2E8\uD55C \uBD80\uBD84\
  \uC744 \uB354 \uAD00\uC6A9\uC801\uC774\uACE0 \uC720\uC9C0\uBCF4\uC218\uAC00 \uC6A9\
  \uC774\uD558\uB3C4\uB85D \uB9AC\uD329\uD1A0\uB9C1\uD574 \uBD05\uC2DC\uB2E4. \uC815\
  \uC218 \uBCA1\uD130\uC758 \uD569\uC744 \uACC4\uC0B0\uD558\uB294 \uD568\uC218\uBD80\
  \uD130 \uC2DC\uC791\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.927403-06:00'
model: gpt-4-0125-preview
summary: "Rust \uCF54\uB4DC\uC758 \uAC04\uB2E8\uD55C \uBD80\uBD84\uC744 \uB354 \uAD00\
  \uC6A9\uC801\uC774\uACE0 \uC720\uC9C0\uBCF4\uC218\uAC00 \uC6A9\uC774\uD558\uB3C4\
  \uB85D \uB9AC\uD329\uD1A0\uB9C1\uD574 \uBD05\uC2DC\uB2E4."
title: "\uB9AC\uD329\uD1A0\uB9C1"
weight: 19
---

## 방법:
Rust 코드의 간단한 부분을 더 관용적이고 유지보수가 용이하도록 리팩토링해 봅시다. 정수 벡터의 합을 계산하는 함수부터 시작합니다:

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("The sum is {}", sum(&numbers));
}
```

출력:
```
The sum is 15
```

이제 이것을 반복자와 `fold` 메서드를 활용하여 더 관용적인 Rust를 사용하도록 리팩토링해 봅시다:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("The sum is {}", sum(&numbers));
}
```

출력 변경 없음—여전히 `15`입니다—하지만 리팩토링된 버전은 더 깔끔하고 Rust의 장점인 borrowing과 반복자 메서드를 사용합니다.

## 심층 분석
리팩토링은 Smalltalk 커뮤니티에서 그 뿌리를 두고 있으며, 마틴 파울러의 책 "Refactoring: Improving the Design of Existing Code"에 의해 자바 세계에서 대중화되었습니다. 그 원칙은 보편적이며 안전성과 동시성이 중요한 Rust에도 적용됩니다. Rust는 컴파일 시간에 문제를 포착하여 견고한 코드 작성을 장려하므로, 리팩토링하는 동안 Rust 컴파일러는 안전망 역할을 합니다.

수동 리팩토링 대안에는 코드 포맷팅을 위한 'rustfmt'나 코드 리팩토링 제안을 할 수 있는 린팅 툴 'clippy'와 같은 자동화 도구 사용이 포함됩니다. 그러나 깊은 리팩토링은 이러한 도구로는 완전히 자동화할 수 없는 코드 디자인에 대한 심도있는 이해를 필요로 합니다.

Rust에서 리팩토링은 타입 사용 개선, 라이프타임을 효과적으로 활용하기, 불필요한 할당 줄이기, 필요할 때 `Arc<Mutex<T>>`와 같은 동시성 패턴 사용하기 등을 중심으로 이루어질 수 있습니다. 또한 `unwrap()`에서 `Result<T, E>`로 더 표현적인 에러 처리로 전환하는 것도 흔합니다.

## 참조
Rust에서의 리팩토링을 더 깊게 다루려면:

- 러스트 책: https://doc.rust-lang.org/book/
- 예제로 배우는 Rust: https://doc.rust-lang.org/rust-by-example/
- Clippy, Rust 린팅 툴: https://github.com/rust-lang/rust-clippy
- "Refactoring: Improving the Design of Existing Code" by Martin Fowler: https://martinfowler.com/books/refactoring.html
