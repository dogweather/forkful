---
title:                "Rust: 현재 날짜 가져오기"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 이유

현재 날짜를 얻는 것은 프로그래밍에서 기본적인 작업 중 하나입니다. 예를 들어, 여러분이 만든 애플리케이션에서 사용자에게 현재 날짜를 보여주고 싶을 때가 있을 것입니다. 또는 이벤트를 만들 때나 파일을 작성할 때 현재 날짜를 포함하고 싶을 수도 있습니다. Rust 언어로 프로그래밍을 하면서 이 작업을 어떻게 수행할 수 있는지 알아보겠습니다.

## 하는 법

Rust에서 현재 날짜를 가져오는 가장 간단한 방법은 `chrono` 라이브러리를 사용하는 것입니다. `chrono`는 날짜 및 시간 관련 작업을 위한 유용한 기능을 제공합니다.

먼저, `chrono` 라이브러리를 사용하기 위해 `Cargo.toml` 파일에 다음을 추가해 주세요.

```
[dependencies]
chrono = "0.4.19"
```

그리고 코드를 작성해 보겠습니다.

```rust
extern crate chrono; // chrono 라이브러리를 가져옵니다
use chrono::{Local, Datelike}; // Local, Datelike 타입을 사용합니다

fn main() {
    let today = Local::today(); // 현재 날짜를 가져옵니다
    println!("오늘은 {}년 {}월 {}일입니다.", today.year(), today.month(), today.day()); // 형식에 맞게 출력합니다
}
```

위 코드를 실행하면 현재 날짜가 출력될 것입니다.

```
오늘은 2021년 3월 21일입니다.
```

`Local::today()` 대신 `Local::now()`를 사용하면 현재 시간까지 출력할 수 있습니다.

이 외에도 `chrono` 라이브러리에는 다양한 날짜 및 시간 관련 기능이 있으니 필요한 경우 공식 문서를 참고해 보세요.

## 깊게 들어가기

Rust는 기본적으로 날짜 및 시간 관련 기능이 제공되지 않기 때문에 `chrono` 라이브러리를 사용해야 합니다. 이 때문에 일부 사용자들은 불편함을 느낄 수도 있지만, 이를 보완하기 위해 Rust 개발팀은 `std::time::SystemTime` 타입을 제공합니다. 이를 사용하면 `chrono` 라이브러리 없이도 현재 시간을 얻을 수 있습니다.

하지만 `chrono` 라이브러리는 보다 다양한 기능을 제공하고 있기 때문에 이를 사용하는 것이 보편적입니다.

## 참고

- [Rust 공식 문서 - 날짜 및 시간 관련 데이터 타입](https://doc.rust-lang.org/std/time/index.html)
- [ chrono 라이브러리 문서](https://docs.rs/chrono/0.4.19/chrono/index.html)