---
title:                "문자열 소문자로 변환하기"
html_title:           "Rust: 문자열 소문자로 변환하기"
simple_title:         "문자열 소문자로 변환하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열을 소문자로 변환하는 것은 프로그래머가 많이 사용하는 작업 중 하나입니다. 이 작업은 주어진 문자열을 관리하기 쉽고 일관성 있게 만들어주며, 데이터베이스 쿼리나 검색 기능에서 대소문자를 구분하지 않는 경우에 유용합니다. 

## 방법:
Rust에서 문자열을 소문자로 변환하는 방법은 매우 간단합니다. 아래 코드를 참조하세요:
```Rust
let my_string = "HELLO";
let lower_case_string = my_string.to_lowercase();
println!("{}", lower_case_string); // 출력 결과: hello
```

## 깊이 파고들기:
문자열을 소문자로 변환하는 기능은 프로그래밍 언어마다 다르게 구현될 수 있습니다. Rust에서는 표준 라이브러리에 포함된 String 타입의 to_lowercase() 메소드를 사용하여 문자열을 소문자로 변환합니다. 이 외에도 다른 언어나 라이브러리에서는 tolower() 함수를 사용하는 경우도 있습니다.

## 참고 자료:
- [Rust 표준 라이브러리 문서: to_lowercase()](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [C++에서 문자열을 소문자로 변환하는 방법](https://www.cplusplus.com/reference/cctype/tolower/)
- [JavaScript에서 문자열을 소문자로 변환하는 방법](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)