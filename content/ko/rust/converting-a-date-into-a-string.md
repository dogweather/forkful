---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

날짜를 문자열로 변환한다는 것은 프로그래밍의 일반적인 작업입니다. 이는 일반적으로 사용자 인터페이스에서 날짜를 보여줄 때, 혹은 날짜 데이터를 그대로 저장하고 싶을 때 사용됩니다. 

## 방법:

Rust에서 날짜를 문자열로 변환하는 방법은 chrono 라이브러리를 사용하는 것입니다. 여기서 어떻게 작동하는지 예시를 보여드리겠습니다.

```Rust
extern crate chrono;
use chrono::{DateTime, Utc};

fn main() {
    let now: DateTime<Utc> = Utc::now();
    println!("{}", now.to_string());
}
```

위 코드를 실행하면 현재 시간을 UTC 날짜로 받아와서 그 값을 문자열로 변환하여 출력합니다.

## 깊은 이해

날짜를 문자열로 변환하는 작업은 프로그래밍 초기부터 존재하였으며, 이를 처리하는 방법은 언어와 라이브러리에 다릅니다. Rust에서는 보통 `chrono` 라이브러리를 활용합니다.

`chrono` 라이브러리는 Rust에서 가장 많이 사용하는 날짜와 시간 처리 라이브러리로, 초보자부터 전문가까지 다룰 수 있는 충분한 기능을 제공합니다. `Utc::now()`를 이용하여 현재 시간을 받아오고, `to_string()` 메서드를 이용하여 이를 문자열로 변환할 수 있습니다.

그러나 Rust의 표준 라이브러리나 다른 서드파티 라이브러리를 사용하여도 동일한 기능을 수행할 수 있습니다.

## 참고 자료

- Rust 공식 문서: https://doc.rust-lang.org/std/
- Chrono 라이브러리 Documentation: https://docs.rs/chrono/0.4.19/chrono/
- Rust by Example – Converting dates to strings: 
https://doc.rust-lang.org/stable/rust-by-example/std_misc/chrono.html