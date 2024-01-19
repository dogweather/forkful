---
title:                "문자열 대문자화"
html_title:           "Rust: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나요?

문자열 대문자 변환은 모든 문자를 대문자로 바꾸는 과정을 의미합니다. 프로그래머들은 사용자 입력 맞춤, 데이터 일관성 유지 등을 위해 이를 사용합니다.

## 어떻게 사용하나요:

다음은 Rust에서 문자열 대문자 변환하는 예시입니다:

```Rust
fn main() {
    let s = "Hello, world!";
    println!("{}", s.to_uppercase());
}
```

출력결과:

```Rust
HELLO, WORLD!
```

이 코드는 'Hello, world!' 문자열을 대문자로 변환하여 'HELLO, WORLD!'를 출력합니다.

## 깊이 있게 살펴보기:

문자열 대문자 변환은 프로그래밍 언어의 역사와 거의 동시에 나왔습니다. 특히, 대소문자를 구분하지 않는 언어 (예: SQL)에서 이 기능은 매우 중요하게 쓰여왔습니다. Rust에서는 'to_uppercase' 메서드를 사용하여 문자열을 대문자로 변환합니다. 이 메서드는 'char' 타입이 아닌 'String' 타입에만 적용됩니다.

그럼에도 불구하고, Rust에서 대문자 변환을 사용하는 다른 방법도 존재합니다. 예를 들면, 'ASCII 대문자 변환'이라는 것이 있는데, 이는 오직 ASCII 문자만 대문자로 변환합니다.

```Rust
use std::ascii::AsciiExt;

fn main() {
    let s = "Hello, world!";
    println!("{}", s.make_ascii_uppercase());
}
```

위 코드는 'Hello, world!'라는 문자열을 'HELLO, WORLD!'라는 대문자로 출력합니다.

## 관련 자료:

- Rust 문서: https://doc.rust-lang.org/std/primitive.str.html
- Rust에서 문자열 다루기: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html#strings-are-always-valid-utf-8