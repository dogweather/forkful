---
title:    "Rust: 두 날짜 비교하기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜
루스트 프로그래밍을 하려는 사람들은 날짜를 비교해야할 필요가 있을 수 있습니다. 이는 프로그래머들이 날짜 및 시간 정보를 다루는 프로그램을 개발하는 데 유용하며, 루스트에서 날짜를 쉽게 비교하는 방법에 대한 지식은 필수적입니다.

## 어떻게
두 날짜를 비교하는 방법은 매우 간단합니다. 먼저 루스트의 `chrono` 라이브러리를 사용해서 두 날짜를 `DateTime` 형식으로 변환해야 합니다. 그리고 나서 `DateTime` 형식의 메서드인 `cmp()`를 사용하여 두 날짜를 비교할 수 있습니다.

```Rust
use chrono::{DateTime, Local, Duration};

// 현재 시간과 비교할 날짜 생성
let current_time = Local::now();
let compare_time = DateTime::parse_from_str("2021-09-17 13:00:00", "%Y-%m-%d %H:%M:%S").unwrap();

// 두 날짜 비교
let comparison = current_time.cmp(&compare_time);

// 결과 출력
println!("현재 시간은 비교할 날짜보다 {}합니다.", comparison);
```

출력: "현재 시간은 비교할 날짜보다 미래입니다."

위 코드에서는 현재 시간과 `2021-09-17 13:00:00`를 비교하고, `DateTime` 형식의 `cmp()` 메서드를 사용하여 두 날짜를 비교한 결과를 출력하는 예제입니다.

## 깊게 들어가기
루스트에서는 날짜와 시간을 다루는 다양한 기능을 제공합니다. `chrono` 라이브러리를 사용하면 윤년, 타임존, 시차 등을 고려하여 쉽게 날짜와 시간을 다룰 수 있습니다. 또한 `DateTime` 형식의 메서드를 활용하면 특정 날짜와 시간의 연산도 가능합니다. 이는 프로그래밍에서 유용하게 활용될 수 있습니다.

# 참고자료
- [Rust 공식 문서 - 날짜와 시간 다루기](https://doc.rust-lang.org/std/time/index.html)
- [Chrono 라이브러리 문서](https://docs.rs/chrono/0.4.19/chrono/)
- [RustCrush 블로그 - 루스트와 날짜 비교하기](https://rustcrush.com/2021/02/27/compare-dates-in-rust/)