---
title:                "현재 날짜 가져오기"
html_title:           "Rust: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜
날짜와 시간은 프로그래밍에서 필수적인 요소입니다. 현재 날짜를 얻는 것은 간단하지만 유용한 기능입니다. 따라서, 이 기능을 사용하는 이유는 매우 간단합니다!

## 하는 방법
Rust의 standard library에는 현재 시간 및 날짜를 얻기 위한 유용한 함수가 있습니다. `chrono` crate를 사용하여 현재 날짜와 시간에 대한 정확한 정보를 얻을 수 있습니다. 아래 예제를 참고하세요:

```Rust
use chrono::{Datelike, Timelike, Utc};

fn main() {
    let now = Utc::now(); // 현재 날짜와 시간 정보를 가져옵니다.

    println!(
        "현재 날짜는 {}년 {}월 {}일 이며, 현재 시간은 {}시 {}분 {}초 입니다.",
        now.year(),
        now.month(),
        now.day(),
        now.hour(),
        now.minute(),
        now.second(),
    );
}
```

만약 당신이 현재 날짜만 필요하다면 `now.date()`를 사용하면 됩니다. 이것은 년도, 월, 일 정보만 포함하며 시간과 관련된 정보는 제외됩니다.

## 깊게 들어가기
`chrono` crate는 여러 다른 날짜와 시간 관련 기능을 제공합니다. 이것은 최신 버전의 Rust code로 작성된 고성능의 방식입니다. 이 crate를 사용하여 현재 날짜와 시간 외에도 다른 날짜와 시간을 다루는 방법을 배울 수 있습니다. 또한, 타임존과 날짜 간의 변환도 가능합니다.

## 관련 자료
- [chrono crate 공식 문서](https://docs.rs/chrono)
- [Rust Programming Language](https://www.rust-lang.org/learn)