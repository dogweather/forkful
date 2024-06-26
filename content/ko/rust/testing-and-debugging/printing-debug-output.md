---
date: 2024-01-20 17:53:45.166811-07:00
description: "How to: (\uBC29\uBC95:) \uAD6C\uC870\uCCB4\uB97C \uB514\uBC84\uADF8\
  \ \uCD9C\uB825\uD558\uB824\uBA74 `Debug` \uD2B8\uB808\uC787\uC744 \uAD6C\uD604\uD574\
  \uC57C \uD55C\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.709288-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95:) \uAD6C\uC870\uCCB4\uB97C \uB514\uBC84\uADF8 \uCD9C\uB825\
  \uD558\uB824\uBA74 `Debug` \uD2B8\uB808\uC787\uC744 \uAD6C\uD604\uD574\uC57C \uD55C\
  \uB2E4."
title: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uCC0D\uC5B4\uBCF4\uAE30"
weight: 33
---

## How to: (방법:)
```Rust
fn main() {
    let my_variable = 10;
    println!("my_variable: {}", my_variable); // 변수 출력
    println!("Debug output: {:?}", my_variable); // {:?}는 디버그 출력 포맷
}

// 출력값:
// my_variable: 10
// Debug output: 10
```

구조체를 디버그 출력하려면 `Debug` 트레잇을 구현해야 한다:

```Rust
#[derive(Debug)]
struct MyStruct {
    name: String,
    age: u8,
}

fn main() {
    let person = MyStruct {
        name: String::from("Alice"),
        age: 30,
    };
    
    println!("Debug output: {:?}", person); // 구조체 디버그 출력
}

// 출력값:
// Debug output: MyStruct { name: "Alice", age: 30 }
```

## Deep Dive (심층 분석):
Rust에서 프린트 디버그는 `println!` 매크로로 시작됐다. 이것은 C언어의 `printf`와 유사하지만, Rust의 소유권 및 타입 안전 기능과 결합됐다.

`{:?}` 포맷은 `Debug` 트레잇의 인스턴스를 출력하도록 요구한다. `Debug`는 자동으로 구현할 수 있는데, `#[derive(Debug)]` 어노테이션을 사용하는 것이다.

`Display` 트레잇은 사용자 친화적인 출력을 위한 것으로, `fmt::Display`를 구현해야 하며, `{}` 포맷 홀더로 사용할 수 있다. 반면에, `Debug`은 개발자에게 유용한 정보를 제공하는데 초점을 맞춘다.

`std::fmt` 모듈은 다양한 포매팅 옵션을 포함하고 있다. 개발자는 필요에 따라 자체 포매팅 행위를 정의할 수 있다.

## See Also (참고 자료):
- Rust 문서의 `std::fmt` 모듈: [https://doc.rust-lang.org/std/fmt/](https://doc.rust-lang.org/std/fmt/)
- Rust By Example의 "Display" 섹션: [https://doc.rust-lang.org/rust-by-example/hello/print/print_display.html](https://doc.rust-lang.org/rust-by-example/hello/print/print_display.html)
- "The Rust Programming Language"의 출력 포매팅 섹션: [https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html#printing-values-with-println-placeholders](https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html#printing-values-with-println-placeholders)
