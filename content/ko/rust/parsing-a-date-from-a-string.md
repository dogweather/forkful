---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:38:54.964181-07:00
simple_title:         "문자열에서 날짜 파싱하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열에서 날짜 파싱은 문자열 형태의 날짜를 실제 날짜 타입으로 변환하는 과정을 말합니다. 프로그래머들은 데이터 처리, 유효성 검사 또는 날짜 연산을 위해 이 과정을 수행합니다.

## 사용 방법:
```Rust
use chrono::{DateTime, NaiveDateTime, Utc};

fn main() {
    let date_str = "2023-03-27T12:45:00Z";
    
    // rfc3339 포맷을 사용하는 예시
    let date: DateTime<Utc> = date_str.parse().expect("Invalid date format");
    println!("Parsed Date & Time: {}", date);

    // 특정 포맷을 지정하는 예시
    let custom_format = "%Y-%m-%d %H:%M:%S";
    let naive_date = NaiveDateTime::parse_from_str("2023-03-27 12:45:00", custom_format)
        .expect("Invalid date format");
    println!("Parsed Naive Date & Time: {}", naive_date);
}

// 출력:
// Parsed Date & Time: 2023-03-27 12:45:00 UTC
// Parsed Naive Date & Time: 2023-03-27 12:45:00
```

## 깊이 들여다보기:
문자열에서 날짜를 파싱하는 기능은 컴퓨터가 시간을 이해하고 처리하는 데 필수적입니다. Rust에서는 `chrono` 크레이트가 대중적으로 이용됩니다. 이전에는 표준 라이브러리의 기능에 더 의존했지만, `chrono`는 더 강력한 기능과 사용 편의성을 제공합니다.

대안으로는 `time` 크레이트 또는 `dateutil`과 같은 여러 서드파티 라이브러리가 있습니다.

구현에 있어서는, 포맷 지정자를 통해 매우 다양한 날짜 형식을 해석할 수 있습니다. `DateTime`, `NaiveDateTime`, `Date`, `Time`과 같은 객체들을 사용해서 날짜와 시간을 나타낼 수 있으며, 라이브러리마다 기능과 표현 방식이 조금씩 다를 수 있습니다.

## 관련 자료:
- [chrono crate documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [The Rust Programming Language Book, date and time handling](https://doc.rust-lang.org/book/ch12-05-working-with-environment-variables.html#storing-values-in-the-environment)
