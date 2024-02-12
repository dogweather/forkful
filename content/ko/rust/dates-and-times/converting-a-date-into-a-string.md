---
title:                "날짜를 문자열로 변환하기"
aliases:
- /ko/rust/converting-a-date-into-a-string/
date:                  2024-01-20T17:38:13.581223-07:00
model:                 gpt-4-1106-preview
simple_title:         "날짜를 문자열로 변환하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜를 문자열로 변환하는 것은 날짜 데이터를 텍스트 형식으로 바꾸는 과정입니다. 사용자에게 날짜 정보를 보여주거나, 날짜를 로깅하고, 다른 시스템과 날짜 데이터를 교환할 때 필요합니다.

## How to: (어떻게 하나요?)
Rust에서 날짜를 문자열로 변환하려면 `chrono` 크레이트를 사용합니다. 예를 들어 보겠습니다:

```Rust
// chrono 크레이트를 사용하기 위한 의존성을 추가해야 합니다.
use chrono::{DateTime, Utc, Local, NaiveDateTime, TimeZone};

fn main() {
    let utc: DateTime<Utc> = Utc::now();
    let local: DateTime<Local> = Local::now();
    
    // UTC 날짜와 시간을 문자열로 변환
    println!("{}", utc.format("%Y-%m-%d %H:%M:%S"));
    
    // 로컬 날짜와 시간을 문자열로 변환
    println!("{}", local.format("%Y-%m-%d %H:%M:%S"));
    
    // NaiveDateTime 사용
    let naive_dt = NaiveDateTime::parse_from_str("2023-03-14T13:59:26", "%Y-%m-%dT%H:%M:%S").unwrap();
    println!("{}", naive_dt.format("%Y-%m-%d %H:%M:%S").to_string());
}

```

출력 예시:

```
2023-03-14 13:59:26
2023-03-14 16:59:26
2023-03-14 13:59:26
```

이렇게 출력된 문자열은 로그, 사용자 인터페이스, 파일 저장 등 여러 곳에서 쓸 수 있습니다.

## Deep Dive (심층 분석)
Rust에서 날짜를 문자열로 변환하는 과정은 `chrono` 크레이트로 이해될 수 있습니다. `chrono`는 Rust 커뮤니티가 만든 날짜와 시간을 다루는 라이브러리로, 시간대를 적용하거나 다양한 형식으로 날짜를 파싱하고 포맷팅 할 수 있습니다. 과거 C++에서 `Boost.DateTime` 또는 Python의 `datetime` 모듈과 유사한 기능을 제공합니다.

대안으로는 Rust 표준 라이브러리인 `std::time`을 사용할 수 있지만, 이는 표현력이 떨어져 복잡한 날짜와 시간 작업에는 제한적입니다. 구현 세부 사항으로는 `chrono`의 `DateTime` 타입이 내부적으로 `NaiveDateTime`과 시간대(`TimeZone`)을 결합하여 사용합니다. 이는 타임존 정보를 갖는 `DateTime`과 그렇지 않은 `NaiveDateTime` 중 선택할 수 있게 합니다.

## See Also (추가 자료)
- [Chrono Crate Documentation](https://docs.rs/chrono)
- [The Rust Programming Language – Official Documentation](https://doc.rust-lang.org/book/)
