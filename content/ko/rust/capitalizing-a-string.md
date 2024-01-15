---
title:                "문자열 대문자로 변환하기"
html_title:           "Rust: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 첫 글자를 대문자로 만드는 것은 때로는 필요할 수 있습니다. 예를 들어, 사용자의 이름을 출력할 때 더 프로페셔널하게 보이게 하거나, 문자열을 정렬하기 위해서입니다.

## 하는 방법

```Rust
fn capitalize_string(str: &str) -> String {
    let mut chars = str.chars();
    match chars.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().chain(chars).collect(),
    }
}

fn main() {
    let s = "hello";
    let result = capitalize_string(s);
    println!("{}", result); // Prints "Hello"
}
```

## 깊이 파고들기

문자열을 대문자로 만드는 과정에서 Rust 언어의 특징 중 하나인 패턴 매칭을 사용했습니다. 또한, Rust의 String 타입과 char 타입에 대해서도 알아보았습니다. 문자열의 첫 글자를 대문자로 만드는 많은 다른 방법이 있지만, 이 방법은 코드가 간결하고 효율적입니다.

## 참고 자료

- [Rust 공식 문서](https://www.rust-lang.org/ko/)
- [Rust 프로그래밍 언어 소개](https://techcrunch.com/2017/07/27/what-is-rust-and-why-is-it-so-popular/)
- [패턴 매칭에 대한 Rust 문서](https://doc.rust-lang.org/book/ch18-00-patterns.html)

### 참조

- 출처: https://github.com/rust-lang/rust
- 라이센스: Apache-2.0