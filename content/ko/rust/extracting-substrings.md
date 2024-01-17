---
title:                "부분 문자열 추출"
html_title:           "Rust: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
"서브스트링 추출"이란 무엇일까요? 이것은 문자열에서 특정 부분을 추출하는 것을 의미합니다. 프로그래머들은 이것을 주로 사용하는 이유는 문자열에서 필요한 정보만을 추출하고자 할 때, 혹은 문자열을 조작하거나 분석할 때 유용하기 때문입니다.

## 어떻게:
```Rust
let my_string = "Rust는 최고입니다!";
let extracted = &my_string[0..4];
println!("{}", extracted); // Output: Rust
```
문자열에서 서브스트링을 추출하기 위해서는 대괄호 안에 시작 인덱스와 끝 인덱스를 입력하면 됩니다. 위의 예시에서는 "Rust"라는 문자열의 첫 번째부터 네 번째까지를 추출해 출력하는 코드입니다.

## 깊이 파보기:
1) 역사적 배경: 서브스트링 추출은 옛날부터 다양한 프로그래밍 언어에서 많이 사용되어 왔습니다. C, Java, Python 등 다양한 프로그래밍 언어에서도 이 기능을 지원하고 있습니다.

2) 대안: 서브스트링 추출을 위한 다른 대안으로는 정규 표현식을 이용하는 것이 있습니다. 정규 표현식은 문자열에서 패턴을 찾아 추출하는데 유용합니다.

3) 구현 세부 사항: Rust는 문자열을 UTF-8로 인코딩하기 때문에 문자열 슬라이스를 사용하여 서브스트링을 추출할 때에도 주의해야 합니다. 인덱스를 직접 입력하는 것 외에도, 문자열 메서드를 이용하여 인덱스를 계산할 수도 있습니다.

## 참고자료:
- Rust 공식 문서: https://www.rust-lang.org/learn
- "Rust 입문서" 출판물: https://doc.rust-lang.org/book/
- "Rust From Zero to Async/Await" 유투브 채널: https://www.youtube.com/channel/UCZVlfirg_pmj4gq0qrupj2Q