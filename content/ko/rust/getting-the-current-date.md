---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:17:12.854205-07:00
simple_title:         "현재 날짜 가져오기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
현재 날짜를 얻는 것은 날짜와 시간 정보를 프로그래밍적으로 가지고 오는 행위입니다. 이는 로깅, 날짜 기반 기능 혹은 사용자 인터페이스를 위해 필수적일 수 있습니다.

## How to: (방법)
```Rust
use chrono::{Local, Datelike};

fn main() {
    let today = Local::today();
    
    println!("Current year: {}", today.year());
    println!("Current month: {}", today.month());
    println!("Current day: {}", today.day());
    // 오늘 날짜를 YYYY-MM-DD 형태로 출력합니다.
    println!("Today's date: {}", today.format("%Y-%m-%d").to_string());
}
```
출력 예시:
```
Current year: 2023
Current month: 3
Current day: 15
Today's date: 2023-03-15
```

## Deep Dive (심층 분석)
초기 Rust에서는 표준 라이브러리에 날짜와 시간을 다루는 기능이 제한적이었습니다. 이를 보완하기 위해, `chrono` 크레이트가 개발되었습니다. `chrono`는 다양한 날짜와 시간 연산을 지원하며, Rust에서 가장 인기 있는 날짜-시간 라이브러리가 되었습니다. `Local::today()`의 경우, 시스템의 로컬 타임존을 사용하여 현재 날짜를 반환합니다. `chrono`는 time zone-aware 연산을 할 수 있어, 다양한 시간대에서 작동하는 애플리케이션에 적합합니다. 

Rust의 표준 라이브러리에서는 `std::time` 모듈을 통해 시간을 다룰 수 있지만, 현재 날짜에 대한 직접적인 지원은 제공하지 않습니다. 따라서, 현재 날짜를 취급할 때는 대부분 `chrono`를 선택하게 됩니다. 

추가로, `chrono`는 날짜-시간 파싱, 포매팅, 계산 등의 다양한 기능을 제공하며, 기존의 `time` 라이브러리의 기능을 확장합니다. `Datelike` 트레이트를 사용하면 연(year), 월(month), 일(day)과 같은 정보를 쉽게 추출할 수 있습니다.

## See Also (추가 자료)
- Rust `chrono` 크레이트 문서: https://docs.rs/chrono
- Rust 공식 `std::time` 모듈 문서: https://doc.rust-lang.org/std/time/index.html
- `time` 크레이트 문서: https://docs.rs/time
