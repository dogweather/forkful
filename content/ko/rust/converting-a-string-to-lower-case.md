---
title:                "Rust: 대소문자 변환하기"
simple_title:         "대소문자 변환하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 바꾸는 것이 왜 중요한지 궁금하신가요? 일반적으로, 문자열의 대문자와 소문자는 시스템에서 구별하기 쉽기 때문입니다. 예를 들어, 사용자 이름 또는 이메일 주소와 같이 대소문자가 정확히 일치해야 할 때가 있을 수 있습니다. 또는 텍스트 데이터를 정렬 또는 비교할 때도 대소문자가 일치해야 합니다. 따라서 문자열을 소문자로 바꾸는 것은 매우 유용하고 필수적인 작업입니다.

## 방법

Rust로 문자열을 소문자로 바꾸는 방법은 간단합니다. 문자열을 표준 라이브러리에서 제공하는 `to_lowercase()` 함수를 사용하면 됩니다. 아래 예제 코드를 보세요.

```Rust
let string = "Hello, Rust";
let lowercase_string = string.to_lowercase();
println!("{}", lowercase_string);
```

위 코드의 출력 결과는 다음과 같습니다.

```Rust
hello, rust
```

출력 결과에서 볼 수 있듯이 `to_lowercase()` 함수는 주어진 문자열의 모든 문자를 소문자로 변환해 줍니다. 또한, 이 함수는 원본 문자열을 변경하지 않고 새로운 소문자 문자열을 반환합니다.

## 깊이 들어가기

Rust의 `to_lowercase()` 함수는 실제로 유니코드 문자열에 대한 UTF-8 인코딩을 처리합니다. 이를 통해 다양한 언어의 문자열도 정확하게 소문자로 변환할 수 있습니다. 또한, 이 함수는 매우 빠르고 안전하게 동작합니다. 따라서 문자열을 소문자로 변환할 때는 항상 표준 라이브러리의 `to_lowercase()` 함수를 사용하는 것이 좋습니다.

## 관련 정보 보기

- Rust 표준 라이브러리 문서: https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase 
- Rust 문자열 자료형: https://doc.rust-lang.org/std/string/index.html