---
title:                "미래나 과거의 날짜 계산하기"
aliases: - /ko/rust/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:32:04.999034-07:00
model:                 gpt-4-1106-preview
simple_title:         "미래나 과거의 날짜 계산하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜 계산이란, 특정 날짜에서 과거나 미래로 일정 기간을 더하거나 뺀 날짜를 찾는 것입니다. 프로그래머들은 예정된 이벤트, 유효기간, 스케줄 관리 등을 다루기 위해 이를 사용합니다.

## 사용 방법:
```Rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now = Utc::now();
    println!("Current time is: {}", now);

    let after_two_weeks = now + Duration::weeks(2);
    println!("Time after two weeks will be: {}", after_two_weeks);

    let two_weeks_ago = now - Duration::weeks(2);
    println!("Time two weeks ago was: {}", two_weeks_ago);
}
```
출력 예시:
```
현재 시각은: 2023-04-14T12:00:00Z
2주 후 시각은: 2023-04-28T12:00:00Z
2주 전 시각은: 2023-03-31T12:00:00Z
```

## 심층 분석
과거에는 표준 라이브러리만으로 날짜 계산을 수행했지만, 여러 시간대를 처리하거나 윤초 같은 복잡한 이슈들 때문에 `chrono` 라이브러리 같은 외부 크레이트 사용이 보통입니다. `chrono`는 시간과 날짜를 다룰 때 유용한 많은 기능들을 제공합니다. 또한, `time` 크레이트 같은 대안도 있으며, 특정 요구사항에 따라 선택할 수 있습니다. 날짜 계산은 내부적으로 시간을 틱(tick)으로 환산하여 처리하며, 복잡한 규칙을 적용하여 정확한 시간을 계산합니다.

## 참고 자료
- Rust `chrono` 크레이트 문서: https://docs.rs/chrono/
- Rust 날짜와 시간 관리 가이드: https://doc.rust-lang.org/book/ch10-02-traits.html
- `time` 크레이트 문서: https://docs.rs/time/
