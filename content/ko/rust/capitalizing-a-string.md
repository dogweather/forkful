---
title:                "Rust: 문자열 대문자화"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

변수나 문자열을 대문자로 바꾸는 작업은 프로그래밍에서 매우 일반적입니다. 대문자로 변환하면 문자열을 비교하는 등 다양한 작업에 유용하게 사용할 수 있기 때문입니다.

## 방법

이번 포스트에서는 Rust 언어를 사용하여 문자열을 대문자로 변환하는 방법을 알아보겠습니다. Rust는 문자열 조작에 매우 효율적이며 내장 함수를 사용하여 간단하게 문자열을 대문자로 바꿀 수 있습니다.

```Rust
fn main() {
    let my_string = String::from("hello, world!");
    let capital_string = my_string.to_uppercase(); // 대문자로 변환
    println!("{}", capital_string); // "HELLO, WORLD!"
}
```

이 코드에서는 먼저 `String::from` 함수를 사용하여 "hello, world!"라는 문자열을 변수에 저장합니다. 그 다음 `to_uppercase()` 함수를 사용하여 변수에 저장된 문자열을 대문자로 변환한 뒤 출력합니다.

## 더 깊게

Rust의 문자열 타입은 `String`과 `&str` 두 가지가 있습니다. `String`은 가변적이며 `&str`은 불변적입니다. 따라서 `String`은 바로 수정할 수 있지만 `&str`은 수정할 수 없기 때문에 `to_uppercase()` 함수를 사용할 수 없습니다. 대신 `to_uppercase()` 함수를 사용하려면 우선 `to_string()` 메서드를 사용하여 `&str`을 `String`으로 변환해야 합니다.

```Rust
fn main() {
    let my_str = "hello, world!";
    let my_string = my_str.to_string(); // &str을 String으로 변환
    let capital_string = my_string.to_uppercase(); // 대문자로 변환
    println!("{}", capital_string); // "HELLO, WORLD!"
}
```

## See Also

- [Rust Documentation: String](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust Documentation: str](https://doc.rust-lang.org/std/primitive.str.html)
- [How to Convert a String to Uppercase using Rust](https://www.techiedelight.com/convert-string-uppercase-rust/)