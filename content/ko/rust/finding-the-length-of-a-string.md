---
title:                "문자열의 길이 찾기"
html_title:           "Rust: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Rust로 문자열의 길이를 찾는 방법

## 무엇 & 왜?
문자열의 길이를 찾는 것은, 단순히 문자열이 얼마나 긴지를 알아내는 것입니다.
프로그래머들은 이를 자주 사용하는데, 예를 들면 입력된 문자열의 유효성을 검사하거나 출력 시 문자열의 길이를 제한하기 위해 사용될 수 있습니다.

## 사용 방법:
아래의 코드 블록에 Rust 언어로 작성된 예제와 그 결과가 포함되어 있습니다. 공백 문자를 포함하여 길이를 계산할 때는 ```len()``` 함수를 사용할 수 있으며, 또는 한국어 문자열의 길이를 계산할 때는 ```chars()``` 함수를 사용합니다.

``` Rust
let input = "안녕하세요";
let len = input.len(); // 한국어는 3 byte 문자로 인식하므로 len은 15가 됩니다.
let chars = input.chars().count(); // chars는 한글을 5개로 인식하므로 5가 됩니다.
println!("len: {}", len);
println!("chars: {}", chars);
```

## 더 깊이 알아보기:
- 이전에는 문자열의 길이를 구하기 위해 루프를 사용하는 등 복잡한 방법들이 존재했으나, Rust에서는 표준 라이브러리의 함수를 통해 더 효율적이고 안전하게 길이를 구할 수 있습니다.
- 다른 언어에서는 문자열 길이를 저장하는 변수나 코드를 따로 작성해야하는 경우가 많지만, Rust는 표준 라이브러리의 함수를 통해 길이를 쉽게 구할 수 있습니다.
- Rust의 ```len()``` 함수는 문자열의 길이를 byte 단위로, ```chars()``` 함수는 Unicode 문자를 기준으로 계산합니다.

## 관련 정보 보기:
- [Rust 표준 라이브러리 문서](https://doc.rust-lang.org/std/string/struct.String.html)
- [StackOverflow에서 Rust 문자열 관련 질문과 답변](https://stackoverflow.com/questions/31238615/how-do-i-get-the-length-of-a-string-in-rust)