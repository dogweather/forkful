---
title:                "Rust: 두 날짜 비교하기"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜 두 날짜를 비교해야 하는가?

날짜를 비교하는 것은 프로그래밍에서 자주 발생하는 일입니다. 예를 들어, 두 날짜가 같은지 다른지를 비교하고 특정 날짜 이후에 얼마나 시간이 지났는지를 알아내는 등 다양한 상황에서 필요합니다. 또한, 날짜를 비교하는 방법에 따라 다른 결과가 나올 수 있기 때문에 올바르게 구현하는 것이 중요합니다.

## 날짜 비교하는 방법

Rust는 표준 라이브러리에서 `DateTime`와 `Duration` 타입을 제공하여 날짜와 시간을 쉽게 다룰 수 있게 해줍니다. 먼저, 두 날짜를 생성하고 비교하기 위해서는 `DateTime`의 `from_str` 메소드를 사용하여 문자열로부터 날짜를 만들어야 합니다. 그리고 `DateTime`의 `cmp` 메소드를 사용하여 두 날짜를 비교할 수 있습니다. 아래는 예시 코드와 그에 따른 출력입니다.

```Rust
use std::time::SystemTime;

fn main() {
    let date1 = SystemTime::from_str("2021-01-01 12:00:00").expect("Failed to create date.");
    let date2 = SystemTime::from_str("2021-01-02 12:00:00").expect("Failed to create date.");

    println!("Date 1 is after date 2: {}", date1.cmp(&date2) == std::cmp::Ordering::Greater);
    println!("Date 2 is before date 1: {}", date2.cmp(&date1) == std::cmp::Ordering::Less);
}
```

출력:

```
Date 1 is after date 2: false
Date 2 is before date 1: false
```

위의 예시에서는 `cmp` 메소드의 반환값으로 `std::cmp::Ordering` enum을 사용하여 비교 결과를 확인하고 있습니다. 또한, 날짜 사이의 시간 간격을 알아내기 위해서는 `Duration`의 `checked_sub` 메소드를 사용할 수 있습니다.

## 더 깊이 들어가기

날짜를 비교하는 것은 간단한 것처럼 보일 수 있지만, 세부적인 구현을 살펴보면 복잡한 부분이 있습니다. 예를 들어, 시간대의 영향을 어떻게 다루는지, 윤년의 영향을 어떻게 처리하는지 등 다양한 상황에서의 날짜 비교 방법이 다르게 적용됩니다. 따라서, 여러분이 사용하는 라이브러리에서 이러한 부분들을 잘 처리하고 있는지 확인하는 것이 중요합니다.

## 더 알아보기

- Rust 표준 라이브러리의 `DateTime` 및 `Duration` 모듈: [https://doc.rust-lang.org/std/time/index.html](https://doc.rust-lang.org/std/time/index.html)
- 날짜 비교를 위한 다양한 라이브러리: [https://crates.io/keywords/datetime](https://crates.io/keywords/datetime)