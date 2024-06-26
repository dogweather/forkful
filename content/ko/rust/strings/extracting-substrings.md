---
date: 2024-01-20 17:46:44.355079-07:00
description: "How to (\uBC29\uBC95) \uBD80\uBD84 \uBB38\uC790\uC5F4\uC744 \uCD94\uCD9C\
  \uD558\uB294 \uAE30\uB2A5\uC740 Rust\uC758 \uCD08\uAE30 \uBC84\uC804\uBD80\uD130\
  \ \uC788\uC5C8\uC2B5\uB2C8\uB2E4. `slice` \uC0AC\uC6A9\uBC95\uC744 \uC798 \uC54C\
  \uC544\uC57C \uBA54\uBAA8\uB9AC \uC548\uC804\uC131\uC744 \uBCF4\uC7A5\uD558\uBA74\
  \uC11C \uD6A8\uC728\uC801\uC744 \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  . `text[start..end]`\uCC98\uB7FC \uBC94\uC704\uB97C \uC9C0\uC815\uD558\uC5EC substrings\uB97C\
  \ \uC5BB\uC744 \uC218 \uC788\uACE0,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.694566-06:00'
model: gpt-4-1106-preview
summary: "`slice` \uC0AC\uC6A9\uBC95\uC744 \uC798 \uC54C\uC544\uC57C \uBA54\uBAA8\uB9AC\
  \ \uC548\uC804\uC131\uC744 \uBCF4\uC7A5\uD558\uBA74\uC11C \uD6A8\uC728\uC801\uC744\
  \ \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
weight: 6
---

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
