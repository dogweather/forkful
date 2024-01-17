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

# 무엇 & 왜?

날짜 비교는 두 날짜가 서로 어떻게 비교되는지 알아보는 것을 의미합니다. 이 기술이 중요한 이유는 프로그래머가 날짜를 비교하여 더 많은 정보를 얻을 수 있기 때문입니다.

## 어떻게:

```Rust
use chrono::{Date, TimeZone, Utc};

// 현재 날짜 및 UTC 시간 가져오기
let now: Date<Utc> = Utc::today();

// 다음 주 일요일이 몇월 몇일인지 계산하기
let next_sunday = Utc.ymd(2022, 11, 6) - now;

// 결과 출력: 1년 2개월 22일
println!("{}", next_sunday);
```

```Rust
// 특정 날짜를 다른 타임존의 날짜로 변환하기
let date = Utc.ymd(2021, 7, 11);
let paris_date = date.with_timezone(&Paris);

// 결과 출력: 2021-07-11T02:00:00Z
println!("{}", paris_date);
```

## 깊이 파헤치기:

이 기능은 Rust의 chrono 라이브러리를 사용하여 구현되었습니다. 이 라이브러리는 날짜 및 시간에 관련된 여러 가지 기능을 제공합니다. 이전에는 Rust에서는 날짜 관리를 위한 별도의 라이브러리가 필요했지만, chrono는 이를 제공하여 개발자들에게 편리함을 제공하였습니다.

또 다른 날짜 비교 방법으로는 Date의 크기를 비교하는 것이 있습니다. 이는 Date가 Ord trait를 구현하기 때문에 가능합니다. 또한 날짜를 비교할 때 Timezone도 고려해야 할 수 있습니다.

## 더 알아보기:

- Rust 공식 문서: [chrono](https://docs.rs/chrono)
- Rust reddit 커뮤니티: [rustlang](https://www.reddit.com/r/rustlang/)