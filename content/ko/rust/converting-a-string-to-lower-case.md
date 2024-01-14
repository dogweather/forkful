---
title:                "Rust: 스트링을 소문자로 변환하는 방법"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것에 참여하는 이유는 다양합니다. 예를 들어, 대소문자를 구분하지 않는 데이터베이스에서 문자열을 비교하거나, 사용자 입력을 처리할 때 대문자를 소문자로 바꾸어 일관된 데이터를 얻을 수 있습니다.

## 어떻게

```rust
fn main() {
    let string = String::from("캐주얼 러스트 프로그래밍 블로그 포스트");

    // 문자열을 소문자로 변환
    let lower = string.to_lowercase();

    println!("{}", lower);
}
```

```
캐주얼 러스트 프로그래밍 블로그 포스트
```

위의 예제 코드를 보면 `String` 타입의 `to_lowercase()` 메소드를 사용하여 문자열을 소문자로 변환하는 방법을 확인할 수 있습니다. 이 메소드는 원래의 문자열을 바꾸지 않고, 이를 복사하여 소문자로 변환한 새로운 문자열을 반환합니다.

## 깊게 파헤치기

문자열을 소문자로 변환하는 것에 대해 더 자세히 알아보겠습니다. Rust에서는 기본적으로 UTF-8 인코딩을 지원합니다. 따라서 유니코드 문자에 대한 올바른 변환을 수행하는 것이 중요합니다. Rust에서는 `String` 타입에 대해 `to_lowercase()` 메소드를 제공하지만, `&str` 타입에 대해서는 `to_lowercase()` 함수를 사용해야 합니다.

또한 Rust는 언어 수준에서 문자열을 변경할 수 없도록 하는 **변수의 불변성** 원칙을 강제합니다. 이는 위의 예제에서 볼 수 있는 것처럼 `to_lowercase()` 메소드가 새로운 문자열을 반환하는 이유입니다. 만약 문자열이 변경되는 것을 허용하고 싶다면, `String` 대신 `String::from_mut()` 함수를 사용하여 가변 참조를 얻어와야 합니다.

## 참고

- [Rust 공식 문서 - `to_lowercase()` 메소드](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [Rust 공식 문서 - `to_lowercase()` 함수](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)