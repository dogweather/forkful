---
title:                "정규 표현식을 사용하는 방법"
html_title:           "Rust: 정규 표현식을 사용하는 방법"
simple_title:         "정규 표현식을 사용하는 방법"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜
정규표현식을 사용하는 이유는 무엇일까요? 정규표현식은 텍스트에서 원하는 패턴을 찾고 추출하거나 대체하고 싶을 때 유용합니다.

## 사용 방법
```Rust
// 정규표현식이 포함된 텍스트를 생성합니다.
let text = "My email address is john@example.com ";

// 이메일 패턴을 정의합니다.
let email_regex = regex::Regex::new(r"([a-z0-9]+)@[a-z]+\.[a-z]{2,3}").unwrap();

// 정규표현식에 일치하는 부분을 찾아서 출력합니다.
if let Some(captures) = email_regex.captures(text) {
    // 일치하는 부분을 추출합니다.
    let email = captures.get(0).unwrap().as_str();
    println!("Extracted email: {}", email);
}
```
```
출력 결과:
Extracted email: john@example.com
```

## 더 깊게 들어가기
정규표현식을 통해 원하는 패턴을 찾는 것뿐만 아니라, 복잡한 패턴을 만들어서 유용한 텍스트 처리 작업도 할 수 있습니다. 예를 들어, 이메일 주소를 추출하는 예제에서 "@"를 기준으로 아이디와 도메인을 분리하거나, 이메일 형식에 맞지 않는 텍스트를 걸러내는 등의 작업이 가능합니다.

## 관련 자료
- 여러분이 더 많은 정보를 얻을 수 있는 Rust 공식 문서입니다. [https://doc.rust-lang.org/book/regular-expressions.html](https://doc.rust-lang.org/book/regular-expressions.html)
- Rust에서 정규표현식을 쉽게 사용할 수 있도록 도와주는 `regex` 크레이트입니다. [https://github.com/rust-lang/regex](https://github.com/rust-lang/regex)
- 실제로 Rust에서 사용되는 정규표현식의 예시들을 살펴볼 수 있는 레포지토리입니다. [https://github.com/rust-lang/regex/tree/master/tests](https://github.com/rust-lang/regex/tree/master/tests)

## 참고자료
- [Rust 공식문서](https://doc.rust-lang.org/book/regular-expressions.html)
- [Rust regex 크레이트](https://github.com/rust-lang/regex)
- [Rust regex 예시 레포지토리](https://github.com/rust-lang/regex/tree/master/tests)