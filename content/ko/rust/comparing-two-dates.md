---
title:                "두 날짜 비교하기"
html_title:           "Rust: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
두 날짜를 비교하는 데 관심이 생기는 이유는 우리가 날짜와 시간 정보를 다룰 때 자주 마주하는 문제 중 하나입니다. 요구하는 기능에 따라 날짜와 시간을 정확하게 비교할 수 있어야 합니다. 이를 위해 Rust 프로그래밍 언어를 사용하여 어떻게 두 날짜를 비교할 수 있는지 알아보겠습니다.

## 어떻게
```Rust
// 필요한 라이브러리 가져오기
use chrono::{DateTime, Utc, Duration};

// 비교할 날짜 생성
let date_one: DateTime<Utc> = Utc::now();
let date_two: DateTime<Utc> = Utc::now() + Duration::days(5); // 5일 후 날짜

// 비교하기
if date_one > date_two {
    println!("첫 번째 날짜가 두 번째 날짜보다 미래입니다.");
} else if date_one < date_two {
    println!("첫 번째 날짜가 두 번째 날짜보다 과거입니다.");
} else {
    println!("두 날짜가 같습니다.");
}
```

위 코드에서 우리가 사용한 라이브러리는 `chrono`입니다. 이 라이브러리는 날짜와 시간을 다루는 많은 기능을 제공해줍니다. 우리는 비교할 날짜를 `DateTime` 형태로 생성하고, `now()`를 사용하여 현재 시간을 가져온 후 `Duration`을 사용하여 5일을 더해 두 번째 날짜를 만들어주었습니다. 이후 `if` 문을 사용하여 두 날짜를 비교하고 적절한 출력을 해주었습니다.

## 딥 다이브
두 날짜를 비교할 때 자주 사용되는 또 다른 기능은 두 날짜간의 차이를 계산하는 것입니다. 이를 위해서는 `Duration`을 사용하여 두 날짜 사이의 차이를 계산해줄 수 있습니다.

```Rust
// 필요한 라이브러리 가져오기
use chrono::{Utc, Duration};

// 비교할 날짜 생성
let date_one: DateTime<Utc> = Utc::now();
let date_two: DateTime<Utc> = Utc::now() + Duration::days(5); // 5일 후 날짜

// 두 날짜 사이의 차이 계산
let difference = date_two - date_one;

println!("두 날짜 사이의 차이는 {} 일입니다.", difference.num_days());
```

위 코드에서 우리는 `Duration`을 사용하여 두 날짜 사이의 차이를 계산하고 `num_days()` 메서드를 사용하여 이를 일 단위로 변환하였습니다.

## 참고 자료
- [chrono 라이브러리 공식 문서](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust 언어 공식 웹사이트](https://www.rust-lang.org/)

---
## 더 읽어보기
- [Rust로 날짜와 시간 다루기](https://opensource.com/article/19/9/date-and-time-manipulation-rust)