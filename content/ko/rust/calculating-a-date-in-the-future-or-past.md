---
title:                "Rust: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜
루스트 프로그래밍을 하는 이유는 다양합니다. 그 중 하나는 미래나 과거의 날짜를 계산하고 싶어서일 수 있습니다.

## 어떻게
먼저, `chrono` 라이브러리를 사용하여 날짜를 처리할 수 있습니다. 다음은 과거 날짜를 계산하는 간단한 예제 코드입니다.

```Rust
use chrono::{NaiveDate, Duration};

fn calculate_date_past(year: i32, month: u32, day: u32) -> String {
    let date = NaiveDate::from_ymd(year, month, day);
    let past = date - Duration::days(365);
    past.format("%Y-%m-%d").to_string()
}

fn main() {
    let result = calculate_date_past(2021, 7, 25);
    println!("{}", result); // Output: 2020-07-25
}
```

위 코드를 실행하면, 현재 날짜보다 365일 전인 과거 날짜가 출력됩니다. 이와 같은 방식으로 미래 날짜를 계산할 수도 있습니다.

## 딥 다이브
날짜를 계산하는 것은 단순한 수학적 계산에 불과하지만, 다양한 오차를 고려해야 합니다. 예를 들어, 윤년을 표현하는 Schotch-Whittemore 규칙이나, 각 달의 일 수를 고려해야 합니다. 또한, 타임존이나 로컬 데이터를 계산에 반영해야 할 수도 있습니다.

## 더 자세한 내용
- [Chrono 라이브러리 문서](https://docs.rs/chrono/0.4.19/chrono/index.html)
- [Rust 언어 공식 문서](https://www.rust-lang.org/ko/learn)
- [날짜 계산 예제 코드](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=4e26a6769200c1333721bd8eab81afc7) 

## 더 참고할 만한 자료
- [Rules for calculating dates in the Gregorian calendar](https://www.timeanddate.com/calendar/aboutgregorian.html)
- [Understanding time zones in Rust](https://blog.logrocket.com/how-to-work-with-time-zones-in-rust/)
- [Handling local date and time in Rust](https://deterministic.space/naive-dates.html)