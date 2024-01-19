---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Rust: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

날짜 계산은 특정 포인트에서 미래나 과거의 날짜를 찾는 것을 의미합니다. 프로그래머들은 날짜 계산을 사용하여 예약 시스템, 통계 데이터, 타임라인 트래킹 등에서 수많은 작업을 수행합니다.

## 어떻게 하는가:

Rust에서 날짜 계산을 하려면 chrono 라이브러리를 사용합니다.

```rust
use chrono::{DateTime, Duration, Local};

fn main() {
    let now: DateTime<Local> = Local::now(); //현재 시각 얻는법

    let future_date = now + Duration::days(5); //5일 뒤 날짜 계산
    println!("Future: {}", future_date);

    let past_date = now - Duration::days(5); //5일 전 날짜 계산
    println!("Past: {}", past_date);
}
```
결과:
```rust
Future: 2022-06-28 09:53:00.212638
Past: 2022-06-18 09:53:00.212638
```
## 심층 분석:

날짜 계산은 그 기원을 초시계 시스템, 즉 컴퓨터 시간의 시작으로 거슬러 올라갑니다. UNIX 시간은 1970년 1월 1일부터 측정된 초로 시간을 나타냅니다. Rust의 chrono 라이브러리도 이 같은 전략을 사용합니다.

대안 방법으로는 time 라이브러리가 있습니다. time 라이브러리는 또한 기능이 풍부하며, 매개변수를 추가/감소해 가며 날짜 계산을 수행합니다.

날짜 계산의 구현에는 주의해야 할 중요한 부분이 있네요. 날짜 간 계산은 그 날의 길이에 따라 다르며, 초는 항상 일정하지 않습니다(윤초 때문에). 그래서 복잡한 날짜 계산을 하려면 이런 모든 상황을 고려해야 합니다.

## 참고 자료:

- Rust 날짜 및 시간 라이브러리, Chrono: https://github.com/chronotope/chrono
- Rust의 시간 라이브러리: https://github.com/rust-lang-deprecated/time