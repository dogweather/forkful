---
date: 2024-01-20 17:51:38.500762-07:00
description: "How to (\uBC29\uBC95) Rust\uC5D0\uC11C \uBB38\uC790\uC5F4 \uBCF4\uAC04\
  \uC740 `format!` \uB9E4\uD06C\uB85C \uB610\uB294 `println!` \uB9E4\uD06C\uB85C\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC \uC218\uD589\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC608\
  \uC81C\uB97C \uD1B5\uD574 \uC0B4\uD3B4\uBD05\uC2DC\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T22:38:55.683051-06:00'
model: gpt-4-1106-preview
summary: "How to (\uBC29\uBC95) Rust\uC5D0\uC11C \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740\
  \ `format!` \uB9E4\uD06C\uB85C \uB610\uB294 `println!` \uB9E4\uD06C\uB85C\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC \uC218\uD589\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC608\
  \uC81C\uB97C \uD1B5\uD574 \uC0B4\uD3B4\uBD05\uC2DC\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## How to (방법)
Rust에서 문자열 보간은 `format!` 매크로 또는 `println!` 매크로를 사용하여 수행할 수 있습니다. 예제를 통해 살펴봅시다.

```Rust
fn main() {
    let name = "세계";
    let greeting = format!("안녕, {}!", name);
    println!("{}", greeting); // 출력: 안녕, 세계!
}
```

`format!` 매크로는 변수를 문자열로 변환하고 해당 값을 새 문자열로 반환합니다. `println!`은 이 작업을 표준 출력으로 바로 보내줍니다.

## Deep Dive (깊이 있게 알아보기)
문자열 보간은 다른 프로그래밍 언어들에서 다양하게 구현되어 있습니다. 예를 들어, 파이썬은 f-strings를, 자바스크립트는 템플릿 리터럴을 사용합니다. Rust의 `format!` 및 `println!` 매크로는 내부적으로 `std::fmt` 모듈을 사용하여 작동합니다. 이러한 매크로들은 타입 안전을 보장하고 컴파일 타임에 문자열의 형식을 검증합니다. 성능에 민감한 상황에서는 미리 알려진 문자열의 구조를 사용함으로써 문자열의 동적 생성 오버헤드를 줄일 수 있습니다.

Rust에서는 또한 매크로 없이 문자열을 이어 붙이거나 'format!' 매크로를 사용하여 문자열을 미리 만들어서 성능을 최적화할 수도 있습니다만, 가독성이나 유지보수 측면에서는 매크로를 사용하는 것이 종종 더 낫습니다.

## See Also (추가 정보)
- Rust Documentation: `format!` Macro
  - [https://doc.rust-lang.org/std/macro.format.html](https://doc.rust-lang.org/std/macro.format.html) 
- Rust Documentation: `println!` Macro
  - [https://doc.rust-lang.org/std/macro.println.html](https://doc.rust-lang.org/std/macro.println.html)
- Rust Book: More about Macros
  - [https://doc.rust-lang.org/book/ch19-06-macros.html](https://doc.rust-lang.org/book/ch19-06-macros.html)
- Rust by Example: Formatted print
  - [https://doc.rust-lang.org/rust-by-example/hello/print.html](https://doc.rust-lang.org/rust-by-example/hello/print.html)
