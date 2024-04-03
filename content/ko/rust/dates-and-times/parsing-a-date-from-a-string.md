---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:28.564131-07:00
description: "\uBC29\uBC95: #."
lastmod: '2024-03-13T22:44:54.929254-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 방법:


### Rust의 표준 라이브러리 사용 (`chrono` 크레이트)
Rust 표준 라이브러리에는 날짜 파싱을 직접적으로 포함하고 있지 않지만, 널리 사용되는 `chrono` 크레이트는 날짜 및 시간 조작을 위한 강력한 해결책입니다. 먼저, `Cargo.toml`에 `chrono`를 추가하세요:

```toml
[dependencies]
chrono = "0.4"
```

그런 다음, `chrono`를 사용하여 날짜 문자열을 `NaiveDate` 객체로 파싱하세요:

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("날짜 파싱 실패");

    println!("파싱된 날짜: {}", date);
}

// 샘플 출력:
// 파싱된 날짜: 2023-04-01
```

### Rust의 고급 날짜-시간 처리 사용 (`time` 크레이트)
더 고급 날짜-시간 처리, 더 편리한 파싱을 포함하고 싶다면, `time` 크레이트를 고려해 보세요. 먼저, `Cargo.toml`에 포함시키세요:

```toml
[dependencies]
time = "0.3"
```

그런 다음, `Date` 타입과 `PrimitiveDateTime`을 사용하여 날짜 문자열을 파싱하세요:

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("날짜 및 시간 파싱 실패");

    println!("파싱된 날짜시간: {}", parsed_date);
}

// 샘플 출력:
// 파싱된 날짜시간: 2023-04-01 12:34:56
```

두 예제 모두 제삼자 크레이트의 도움으로 Rust가 날짜 문자열을 조작 가능한 날짜 객체로 파싱하는 것을 촉진하여, 시간 데이터를 포함하는 소프트웨어 개발을 위한 강력한 도구임을 보여줍니다.
