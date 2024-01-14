---
title:                "Rust: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜 Rust로 날짜 비교를 해야 할까요?

날짜를 비교하는 것은 프로그래밍에서 자주 사용되는 일입니다. 예를 들어, 두 날짜를 비교하여 더 큰 날짜를 찾거나, 특정 날짜의 과거나 미래 여부를 확인하는 등의 작업이 있을 수 있습니다. 이러한 날짜 비교를 효율적이고 안전하게 할 수 있는 언어 중 하나가 바로 Rust입니다.

## 어떻게 Rust로 날짜 비교를 할 수 있을까요?

Rust에는 `DateTime` 구조체가 있어 날짜와 시간을 표현할 수 있습니다. 또한 `DateTime` 구조체는 `PartialOrd` 트레이트를 구현하여 비교가 가능하도록 해줍니다. 따라서 날짜 비교를 위해서는 먼저 `DateTime` 구조체를 생성하고 `PartialOrd` 트레이트를 사용하여 비교하면 됩니다.

```rust
use chrono::DateTime;
use chrono::offset::Utc;

let today = DateTime::parse_from_rfc3339("2021-10-15T00:00:00+00:00").unwrap(); // 예시로 오늘의 날짜를 생성
let tomorrow = today + chrono::Duration::days(1); // 오늘의 날짜에서 1일을 더하여 내일의 날짜 생성

if today < tomorrow {
    println!("내일은 오늘보다 미래입니다.");
}
```
위 코드에서 `chrono` 라이브러리를 사용하여 `DateTime` 구조체를 생성하고, `PartialOrd` 트레이트를 이용하여 두 날짜를 비교하고 있습니다. 이처럼 매우 간단하게 Rust를 이용하여 날짜 비교를 할 수 있습니다.

## 깊게 들어가보면?

실제로는 날짜를 비교할 때 시간대(Timezone)와 윤초(Leap Seconds)를 고려해야 합니다. 이는 `DateTime` 구조체에 기본적으로 적용되는 `Utc` 타임존을 사용하면 해결할 수 있습니다. 또한, `PartialOrd` 트레이트가 아니라 `Ord` 트레이트를 사용하면 좀 더 정확한 비교가 가능합니다. 자세한 내용은 [Rust 공식 문서](https://doc.rust-lang.org/std/time/struct.DateTime.html)를 참고하시기 바랍니다.

## 배우고 더 알아보기

- [Rust 공식 문서 - DateTime 구조체](https://doc.rust-lang.org/std/time/struct.DateTime.html)
- [Rust By Example - Dates and Times](https://doc.rust-lang.org/stable/rust-by-example/std/time.html)
- [Rust Cookbook - Comparing Dates](https://rust-lang-nursery.github.io/rust-cookbook/datetime/comparing_dates.html)

# 더 자세히 알아보기

오늘 우리는 Rust를 이용하여 날짜 비교를 살펴보았습니다. Rust의 강력한 `DateTime` 구조체와 `PartialOrd` 트레이트를 이용하면 날짜를 간단하고 안전하게 비교할 수 있습니다. 하지만 실제로는 시간대와 윤초 등을 고려해야 한다는 점을 잊지 말아야 합니다. Rust를 공부하는 여러분들의 프로그래밍 실력을 더욱 향상시킬 수 있는 유용한 주제였기를 바랍니다.

# 관련 자료

- [Rust 공식 문서 - DateTime 구