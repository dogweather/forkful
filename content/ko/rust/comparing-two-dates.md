---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

두 날짜를 비교한다는 것은 클래스나 객체의 두 인스턴스가 더 이른 시점 또는 나중 시점을 나타내는지 결정하는 것입니다. 프로그래머는 이것을 예약, 스케줄링 등의 용도로 활용합니다.

## 사용법:

```Rust
use std::cmp::Ordering;
use chrono::{DateTime, Utc};

let date1: DateTime<Utc> = Utc.ymd(2020, 7, 8).and_hms(9, 10, 11);
let date2: DateTime<Utc> = Utc.ymd(2020, 7, 8).and_hms(9, 10, 12);

match date1.cmp(&date2) {
    Ordering::Less => println!("date1은 date2보다 빠릅니다"),
    Ordering::Greater => println!("date1은 date2보다 늦습니다"),
    Ordering::Equal => println!("date1과 date2는 동일한 시간입니다"),
}
```

## 디프다이브:

두 날짜의 비교 시작은 프로그래밍이 컴퓨터 과학에 처음 도입된 이래로 항상 핵심 주제였습니다. 다른 언어(예: Perl, Python 등)는 자체 날짜 비교 메소드를 가지고 있지만 Rust는 Chrono 외부 라이브러리를 사용하여 이를 이해할 수 있습니다.

`cmp` 는 Ordering enum(순서 열거형)을 반환합니다. 이것은 Less, Equal, 또는 Greater 중 하나입니다. 이렇게 하면 두 날짜를 비교하고 그 결과에 따라 다른 작업을 수행할 수 있습니다.

대체 방법으로는 `PartialEq` 및 `Eq` 트레잇을 사용하거나 `std::time::SystemTime` 을 사용하는 것이 있습니다. 하지만 이러한 방법은 위의 예에서와 같이 명확한 비교가 필요할 경우 유용하지 않을 수 있습니다.

## 추가 자료:

- Rust와 날짜 시간 처리: https://lucumr.pocoo.org/2014/1/5/unicode-in-2-and-3/ 
- Rust 공식 문서 날짜 및 시간 API: https://doc.rust-lang.org/stable/std/time/ 
- Rust와 Chrono 라이브러리: https://docs.rs/chrono/0.4.11/chrono/