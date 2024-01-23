---
title:                "부분 문자열 추출"
date:                  2024-01-20T17:46:44.355079-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 부분 문자열(substrings)을 추출하는 것은 문자열의 특정 부분만 사용하고 싶을 때 필요합니다. 데이터를 파싱하거나 사용자 입력을 처리할 경우에 자주 사용되죠.

## How to (방법)
```Rust
fn main() {
    let text = "안녕하세요, Rust 프로그래밍!";
    let start = 6; // 시작 인덱스입니다.
    let end = 11; // 끝 인덱스입니다.

    let substring = &text[start..end];

    println!("부분 문자열: {}", substring);
    // 부분 문자열: Rust
}
```

## Deep Dive (심층 분석)
부분 문자열을 추출하는 기능은 Rust의 초기 버전부터 있었습니다. `slice` 사용법을 잘 알아야 메모리 안전성을 보장하면서 효율적을 사용할 수 있습니다. `text[start..end]`처럼 범위를 지정하여 substrings를 얻을 수 있고, `text.get(start..end)`라고도 할 수 있습니다.

스트링 슬라이스는 원본 데이터를 복제하지 않고 참조만 합니다. 그래서 메모리 사용을 줄일 수 있습니다. 대신, 올바른 문자 경계에서 슬라이싱하는 것이 중요합니다. 바이트 인덱스가 문자 경계에 일치하지 않으면 패닉(panic)이 발생할 수 있습니다. 예를 들어, UTF-8 문자열에서 한글이나 다른 멀티바이트 문자를 처리할 때 주의가 필요합니다.

`str::get(..)`와 `String::from()`과 같은 몇 가지 안전한 대안도 있습니다. 이들은 실패할 경우 `None`을 반환하거나 새 String 객체를 생성합니다.

## See Also (추가 자료)
- [Rust 공식 문서: The Rust Programming Language - Slices](https://doc.rust-lang.org/book/ch04-03-slices.html)
- [Rust by Example: String slices](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Rust Playground](https://play.rust-lang.org/) - 여러분의 코드를 시험해 볼 수 있는 온라인 에디터입니다.
