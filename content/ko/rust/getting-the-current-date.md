---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

현재 날짜를 얻는 것은 시스템에서 가장 최근의 날짜를 가져오는 것을 의미합니다. 이 기능은 파일의 타임스탬프를 업데이트하거나 사용자에게 현재 시간을 표시하는 등, 다양한 프로그램에서 사용됩니다.

## 어떻게:

아래 코드는 Rust로 현재 날짜를 가져오는 방법을 보여줍니다.

```Rust
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

fn main() {
    let now = SystemTime::now();
    let since_the_epoch = now.duration_since(UNIX_EPOCH)
        .expect("Time went backwards");
    println!("{:#?}", since_the_epoch);
}
```

이 코드를 실행하면, 다음과 같은 출력이 됩니다.

```
379106510.8419099s
```

## 깊게 이해하기

'현재 날짜 가져오기'의 구현은 시스템 시간에서 시작됩니다. 시스템 시간은 연, 월, 일, 시, 분, 초를 상대적인 정보로 변환합니다. Rust에서는 `SystemTime::now()` 함수를 사용하여 현재 시스템 시간을 얻을 수 있습니다.

Historically, getting the current date and time was a function of the operating system, which tracked time since an epoch - a point where time begins. In Unix systems, this epoch is January 1, 1970.

Rust에서 현재 날짜를 가져오는 대안으로는 다양한 외부 라이브러리가 있습니다. `chrono`와 `time` 라이브러리는 보다 분명한 API를 제공하며, 표준 라이브러리에 없는 더 많은 기능을 제공합니다.

## 참고하셔도 좋은 자료

- [Rust Documentation: std::time](https://doc.rust-lang.org/std/time/)
- [chrono library](https://docs.rs/chrono/0.4.19/chrono/)
- [time library](https://docs.rs/time/0.1.44/time/)