---
title:                "스트링의 길이를 찾는 방법"
html_title:           "Rust: 스트링의 길이를 찾는 방법"
simple_title:         "스트링의 길이를 찾는 방법"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜 문자열의 길이를 찾아야 할까요?

문자열의 길이를 알아내는 것은 프로그래밍에서 매우 중요한 일입니다. 예를 들어, 입력한 패스워드가 일정 길이를 초과하는지 확인할 때 문자열의 길이가 필요합니다. 따라서 프로그래밍을 하면서 문자열의 길이를 알아내는 기능은 꼭 필요한 일입니다.

## 어떻게 하나요?

우선, Rust에서 문자열의 길이를 알아내려면 `len()` 함수를 사용해야 합니다. 이 함수는 해당 문자열의 길이를 반환합니다. 아래는 `len()` 함수를 사용하여 문자열 "안녕하세요"의 길이를 알아내는 예제 코드입니다.

```rust
let string = "안녕하세요";
let length = string.len(); // 문자열의 길이를 변수에 저장
println!("길이: {}", length); // 출력 결과: "길이: 5"
```
위 코드에서 볼 수 있듯이 `len()` 함수를 사용하기 위해선 문자열을 변수에 할당해야 합니다. 그리고 해당 변수를 이용하여 `len()` 함수를 호출하여 문자열의 길이를 알아낼 수 있습니다.

## 깊게 파헤쳐 보기

Rust에서 문자열의 길이를 알아내는 방법은 여러가지가 있습니다. 위 예제에서는 `len()` 함수를 사용하였지만 `chars()`를 사용해도 문자열의 길이를 알아낼 수 있습니다. `chars()`는 문자열의 길이를 문자 단위로 반환하므로 한글의 경우 모음이나 자음 개수를 알아낼 수 있습니다. 이 외에도 `bytes()` 함수를 사용하면 문자열의 길이를 바이트 단위로 반환하므로 문자열 내부의 다양한 문자를 분석하는 데에도 유용합니다.

# 참고자료

- [Rust 공식 문서 - String](https://doc.rust-lang.org/std/string/index.html)
- [Rust by Example - Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [Rust Programming Language - String Length](https://www.tutorialspoint.com/rust/rust_string_length.htm)

## 더 보기

- [Rust퀴즈 - 문자열 길이 알아내기](https://rustquiz.com/ko/strings/length.html)
- [Rust 커뮤니티 포럼](https://forum.rustkr.org/)