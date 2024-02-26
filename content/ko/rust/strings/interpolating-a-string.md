---
date: 2024-01-20 17:51:38.500762-07:00
description: "\uBB38\uC790\uC5F4\uC744 \uBCF4\uAC04\uD558\uB294 \uAC83\uC740 \uBB38\
  \uC790\uC5F4 \uC548\uC5D0 \uBCC0\uC218\uC758 \uAC12\uC744 \uC9D1\uC5B4\uB123\uB294\
  \ \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uCF54\
  \uB4DC\uC758 \uAC00\uB3C5\uC131\uC744 \uB192\uC774\uACE0 \uBB38\uC790\uC5F4\uC744\
  \ \uB3D9\uC801\uC73C\uB85C \uC0DD\uC131\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-02-25T18:49:51.903536-07:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC744 \uBCF4\uAC04\uD558\uB294 \uAC83\uC740 \uBB38\uC790\
  \uC5F4 \uC548\uC5D0 \uBCC0\uC218\uC758 \uAC12\uC744 \uC9D1\uC5B4\uB123\uB294 \uACFC\
  \uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uCF54\uB4DC\
  \uC758 \uAC00\uB3C5\uC131\uC744 \uB192\uC774\uACE0 \uBB38\uC790\uC5F4\uC744 \uB3D9\
  \uC801\uC73C\uB85C \uC0DD\uC131\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
문자열을 보간하는 것은 문자열 안에 변수의 값을 집어넣는 과정입니다. 프로그래머들은 코드의 가독성을 높이고 문자열을 동적으로 생성하기 위해 이를 사용합니다.

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
