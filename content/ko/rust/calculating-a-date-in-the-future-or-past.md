---
title:                "미래 또는 과거 날짜 계산하기"
html_title:           "Rust: 미래 또는 과거 날짜 계산하기"
simple_title:         "미래 또는 과거 날짜 계산하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

예를 들어, 생일이나 여행을 계획할 때 앞뒤로 날짜를 계산하는 경우가 있을 것입니다. 또는 현재 날짜를 기반으로 특정 기간 후의 날짜를 계산해야 할 때가 있을 수 있습니다. 이번에는 Rust를 사용하여 미래나 과거의 날짜를 계산하는 방법에 대해 알아보겠습니다.

## 방법

우선, 계산에 필요한 라이브러리를 가져와야 합니다. Rust에서는 `chrono` 라이브러리를 사용하면 됩니다. 먼저 해당 라이브러리를 `Cargo.toml`에 추가해 주세요.
```Rust
[dependencies]
chrono = "0.4.19"
```

그리고 `main.rs` 파일에서 해당 라이브러리를 가져옵니다.
```Rust
use chrono::{Local, DateTime, Duration};
```

이제 `DateTime` 구조체를 사용하여 현재 날짜와 시간을 가져옵니다.
```Rust
let now = Local::now();
```

다음으로는 `Duration`을 사용하여 시간 간격을 계산합니다. 예를 들어, 3일 후의 날짜를 계산하는 경우 코드는 다음과 같습니다.
```Rust
let future_date = now + Duration::days(3);
```

여기서 중요한 점은 미래 날짜를 계산할 때는 `+` 연산자를 사용하고, 과거 날짜를 계산할 때는 `-` 연산자를 사용한다는 것입니다.

아래는 전체 코드와 실행 결과입니다.
```Rust
use chrono::{Local, DateTime, Duration};

fn main() {
    let now = Local::now();
    println!("현재 날짜와 시간: {}", now);

    let future_date = now + Duration::days(3);
    println!("3일 후 날짜: {}", future_date);
}
```

실행 결과는 다음과 같습니다.
```bash
현재 날짜와 시간: 2021-11-01T10:30:00.123456789+09:00
3일 후 날짜: 2021-11-04T10:30:00.123456789+09:00
```

## 딥 다이브

`DateTime` 구조체에는 다양한 메서드가 있어서 우리가 원하는 날짜와 시간을 계산할 때 유용합니다. 예를 들어, `Duration`만큼의 시간 간격을 뺀 날짜를 계산하는 메서드는 `sub`이며, `DateTime` 타입을 `&'a DateTime<Local>`와 같은 형태로 받아들입니다. 또는 `DateTime` 타입을 `format()` 메서드를 사용하여 원하는 형식으로 출력할 수도 있습니다.

## See Also

- [Rust 공식 홈페이지](https://www.rust-lang.org/ko)
- [Rust Cookbook: 날짜 및 시간 계산하기](https://rust-lang-nursery.github.io/rust-cookbook/datetime.html)