---
title:    "Rust: 날짜를 문자열로 변환하기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것의 중요성은 프로그램에서 날짜를 다루는 일상적인 과정입니다. 따라서 이 과정을 효율적이고 정확하게 처리할 수 있는 언어인 Rust를 배우는 것은 큰 도움이 될 것입니다.

## 하는 법

가장 간단한 방법은 Rust 표준 라이브러리의 `format` 매크로를 사용하는 것입니다. 아래 코드 블록을 보면서 예시를 살펴보겠습니다.

```Rust
use std::time::SystemTime;

let time = SystemTime::now();
let date = match time.duration_since(SystemTime::UNIX_EPOCH) {
    Ok(duration) => duration.as_secs(),
    Err(_) => panic!("An error occurred."),
};

println!("Current date: {}", date.format("%Y-%m-%d"));
```

위 코드에서 `SystemTime`을 이용해 현재 시간을 얻고 날짜를 계산하게 됩니다. 그리고 `format` 매크로를 사용하여 `date` 변수의 값을 문자열로 변환하고 출력합니다. 이때 `%Y-%m-%d` 형식 지정자를 사용하면 년-월-일 형식으로 날짜를 출력할 수 있습니다.

다른 형식으로 출력하고 싶다면 Rust 표준 라이브러리의 `chrono` 라이브러리를 사용하여 더 다양한 날짜 형식을 지정할 수 있습니다. 예를 들어 `%a, %b %e %Y` 형식을 사용하면 "Mon, Oct 21 2019"과 같이 날짜를 출력할 수 있습니다.

## 깊게 파고들기

날짜를 문자열로 변환하는 과정에서 주의해야 할 점은 날짜 형식 지정 중 지역 시간대를 반영하는 것입니다. 이 경우 `Local` 타입을 이용하여 날짜를 변환해야 지역 시간대를 정확하게 반영할 수 있습니다. `chrono` 라이브러리에서 제공하는 `Local` 타입을 사용하는 예시를 살펴보겠습니다.

```Rust
use chrono::prelude::*;

let date = Local::now();
println!("Local date: {}", date.format("%c"));
```

위 코드에서 `Local::now()`를 이용해 현재 시간과 날짜를 얻을 수 있습니다. 그리고 `%c` 형식 지정자를 사용하여 날짜와 시간을 함께 출력할 수 있습니다.

## 더 보기

- [Rust 표준 라이브러리 공식 문서](https://doc.rust-lang.org/std/index.html)
- [Chrono 라이브러리 공식 문서](https://docs.rs/chrono/0.4.9/chrono/)
- [Rust 언어 공식 웹사이트](https://www.rust-lang.org/ko)