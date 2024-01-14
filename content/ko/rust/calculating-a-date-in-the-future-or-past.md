---
title:    "Rust: 미래나 과거의 날짜를 계산하기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거에 계산하는 것에 참여해야 하는 이유는 무엇일까요? 간단히 설명해보겠습니다.

우리는 삶에서 항상 시간과 날짜에 의존합니다. 예를 들어, 우리는 옷의 환불 기간을 계산하거나 예약한 여행의 출발 날짜를 확인합니다. 이러한 상황에서 우리가 다루는 날짜는 미래나 과거일 수도 있지만, 그 중 하나를 정확하게 계산해야 합니다.

이러한 이유로 날짜를 계산하는 과정은 우리 삶에서 중요한 역할을 합니다. 이제는 이 작업을 Rust 프로그래밍으로 어떻게 할 수 있는지 알아보겠습니다.

## 어떻게

날짜를 계산하는 것은 프로그래밍에서 중요한 부분입니다. Rust는 이를 수행하는 강력한 기능을 제공합니다. 아래에는 Rust 코드 블록을 통해 예제와 결과를 보여드리겠습니다.

```Rust
use chrono::{Duration, Local, NaiveDate, NaiveDateTime};

// 오늘 기준 5일 후의 날짜 계산
let today = Local::today();
let five_days_later = today + Duration::days(5);
println!("오늘은 {} 이고, 5일 후는 {} 입니다.", today, five_days_later);

// 지정한 날짜의 1년전 날짜 계산
let date = NaiveDate::from_ymd(2019, 4, 20);
let one_year_ago = date - Duration::days(365);
println!("2019년 4월 20일로부터 1년 전은 {} 입니다.", one_year_ago);

// 현재 시간(시, 분, 초)과 30분 뒤 시간 계산
let now = Local::now();
let future_time = now + Duration::minutes(30);
println!("현재 시간은 {} 이고 30분 뒤 시간은 {} 입니다.", now, future_time);
```

위 코드를 실행하면 다음과 같은 결과가 나옵니다.

```
오늘은 2021-10-06 이고, 5일 후는 2021-10-11 입니다.
2019년 4월 20일로부터 1년 전은 2018-04-20 입니다.
현재 시간은 2021-10-06 12:00:00 이고 30분 뒤 시간은 2021-10-06 12:30:00 입니다.
```

이러한 간단한 예제를 통해 Rust를 사용하여 날짜를 계산하는 방법을 배웠습니다. 그렇다면 이것보다 더 심화된 부분을 살펴보겠습니다.

## 더 깊게 들어가기

날짜 계산은 실제로는 매우 복잡한 작업입니다. 이 작업에서 우리는 윤년, 유닉스 타임스탬프, 여러 시간대 등 많은 요소들을 고려해야 합니다. Rust를 사용하여 이러한 요소들을 쉽게 처리할 수 있습니다.

예를 들어, 유닉스 타임스탬프는 1970년 1월 1일부터 시작되는 타임스탬프를 의미합니다. 이것을 Rust와 chrono 라이브러리를 사용해서 계산해보겠습니다.

```Rust
use chrono::{DateTime, NaiveDateTime, Utc};

// 유닉스 타임스탬프로 10일 후 날짜 계산
let timestamp = 163351200