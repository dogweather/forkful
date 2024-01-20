---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

# 서브스트링 추출하기: Rust 프로그래밍
**서브스트링 추출하기**에 대해 배워봅시다. Rust에서 서브스트링 추출하는 방법을 친절하게 설명해드릴게요.

## 서브스트링 추출, 그게 뭐지? 왜 필요해?

서브스트링 추출이란, 문자열의 일부분을 추출하는 행동을 말합니다. 이를 통해 프로그래머들은 문자열의 지정된 부분에만 접근하고 조작할 수 있어요.

## 어떻게 해야할까?
Rust에서 서브스트링을 추출하는 방법은 아래의 예제를 참고하세요.

```Rust
fn main() {
    let my_string = "안녕하세요, Rust 프로그래밍!";
    let hello = &my_string[0..5];
    let programming = &my_string[12..15];
    println!("{}", hello); // "안녕하세요"
    println!("{}", programming); // "프로그래밍"
}
```
`[a..b]` 구문을 이용하면 `a`번째 인덱스부터 `b`번째 인덱스까지의 문자열을 얻을 수 있어요. 그 결과는 새 문자열의 참조로 반환됩니다.

## 더 깊게 알아보기
서브스트링의 추출은 문자열 조작의 기본이며, 많은 프로그래밍 언어에서 공통적으로 제공하고 있습니다.

Rust의 대안적인 서브스트링 추출 방법으로는 `char_indices` 함수를 사용하는 방법이 있어요. `char_indices` 함수는 각 문자와 그 인덱스를 반복자로 제공합니다. 
그러나, 위에서 제시된 방법이 보통 가장 쉽고 빠르게 사용할 수 있습니다.

Rust에서 이용할 수 있는 다른 문자열 조작 기능들도 많이 있으니, 필요에 따라 탐색해보세요.

## 참고하기
더 많은 정보와 예제를 참고하려면 아래 링크를 참조하세요.

- Rust 공식 문서: [문자열 조작](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust의 `str` 타입 API 문서](https://doc.rust-lang.org/std/primitive.str.html)