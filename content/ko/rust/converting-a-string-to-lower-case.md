---
title:                "문자열을 소문자로 변환하기"
html_title:           "Rust: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것이 중요하다고 생각되지 않을 수 있습니다. 하지만 실제로 많은 상황에서 문자열을 소문자로 변환해야 할 필요가 있습니다. 예를 들어 사용자 입력 값을 검증하는 경우, 대문자와 소문자를 구분하지 않는 경우가 많기 때문에 문자열을 소문자로 변환하여 비교하는 것이 유용합니다. 또한, 검색 기능을 구현할 때도 입력된 검색어와 저장된 데이터의 문자열을 소문자로 변환하여 정확한 결과를 보여주는 것이 중요합니다.

## 하우 투

기본적으로 Rust는 아스키(ASCII) 문자열만 지원합니다. 따라서, 문자열의 제한된 길이의 메모리를 어떻게 사용할지 고민할 필요가 없습니다. 마찬가지로, 문자열을 소문자로 변환하는 메서드인 `to_lowercase()`를 사용하면 쉽게 소문자 문자열로 변환할 수 있습니다. 아래는 간단한 예제 코드입니다.

```rust
let string = String::from("Hello, World!");
let lower = string.to_lowercase(); // "hello, world!" 출력
```

`to_lowercase()` 메서드는 인자로 `self`를 갖고 있어서 원본 문자열을 소문자로 바꾸는 것이 아니라, 새로운 문자열을 생성합니다. 따라서 예제 코드에서 `string` 변수의 값은 변하지 않습니다. 이 외에도 `to_lowercase()` 메서드는 UTF-8 인코딩을 지원하며, 아스키 문자 이외의 문자도 올바르게 변환합니다.

## 딥 다이브

문자열을 소문자로 변환하는 것은 간단한 작업처럼 보이지만, 실제로는 문자열을 UTF-8 인코딩으로 파싱한 뒤, 각각의 문자를 대소문자 형태로 변경하는 과정을 거칩니다. 따라서, 문자열이 길어지면 성능에 영향을 미칠 수 있습니다. 이를 피하기 위해서는 문자열을 미리 UTF-8로 변환하여 소문자로 변환하는 것이 좋습니다.

또한, `to_lowercase()` 메서드는 로케일과 매칭되는 대소문자 변환 테이블을 사용합니다. 예를 들어, 한글에서는 대소문자 개념이 없기 때문에, 한글 문자열을 `to_lowercase()` 메서드를 사용해도 변환이 되지 않습니다. 따라서 다른 언어의 문자열을 변환할 때는 주의해야 합니다.

## 관련 링크

- [The Rust Programming Language](https://www.rust-lang.org/)
- [Rust Standard Library](https://doc.rust-lang.org/std/)
- [UTF-8 Encoding](https://en.wikipedia.org/wiki/UTF-8)
- [Locale-Sensitive Operations in Rust](https://doc.rust-lang.org/std/locale/index.html)

## 참고 자료

- [Rust by Example](https://doc.rust-lang.org/rust-by-example/index.html)
- [Discover Rust: Learn the Basics and Advanced Concepts](https://github.com/pearofducks/Discovering_Rust)