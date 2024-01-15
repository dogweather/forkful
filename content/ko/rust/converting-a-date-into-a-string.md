---
title:                "날짜를 문자열로 변환하기"
html_title:           "Rust: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 일이 왜 필요한지 궁금하신가요? 예를 들어, 데이터베이스에 날짜를 저장해야 할 때 혹은 사용자에게 보여줄 때, 날짜를 읽기 쉬운 형식으로 표현하고 싶을 때 등 다양한 이유로 날짜를 문자열로 변환하는 작업이 필요합니다.

## 어떻게

Rust는 강력한 타입 시스템과 안정적인 성능으로 많은 개발자들에게 인기가 있습니다. 날짜를 문자열로 변환하는 방법 또한 간단하고 직관적이며 안전한 방법으로 제공됩니다.

우선, `chrono` 라이브러리를 `Cargo.toml` 파일에 추가해줍니다. 그리고 `use chrono::prelude::*;` 로 라이브러리를 가져온 후 `Local::now().to_rfc3339()`를 호출하여 현재 시간을 RFC3339 형식의 문자열로 가져올 수 있습니다.

```rust
use chrono::prelude::*;

fn main() {

    let date = Local::now().to_rfc3339();
    println!("Converted Date: {}", date);

}
```

출력 예시:

`Converted Date: 2021-08-24T07:21:54+09:00`

이번에는 날짜를 사용자 정의 형식으로 변환해보겠습니다. `format!` 매크로를 사용하여 원하는 형식으로 날짜를 변환할 수 있습니다.

```rust
use chrono::prelude::*;

fn main() {

    let date = Local::now();
    let custom_date = format!("{}-{}-{}", date.year(), date.month(), date.day());
    println!("Converted Date: {}", custom_date);

}
```

출력 예시:

`Converted Date: 2021-8-24`

## 딥 다이브

날짜를 문자열로 변환하는 과정에서 자주 사용되는 타입으로는 `DateTime<Utc>`, `DateTime<Local>`, `NaiveDateTime` 등이 있습니다. 각 타입은 다른 시간대와 날짜를 다룰 수 있지만, 정확한 출력을 위해서는 적절한 타입을 선택하고 변환해야 합니다.

또한 `chrono` 라이브러리 외에도 `time` 라이브러리를 사용하여 날짜와 시간을 다룰 수 있습니다. 이 라이브러리는 다양한 기능을 제공하며, 개발자가 원하는 방식으로 날짜를 변환할 수 있도록 유연성을 제공합니다.

## 더 알아보기

- [Chrono Documentation](https://docs.rs/chrono/0.4.19/chrono/) - `chrono` 라이브러리의 공식 문서입니다.
- [Time Documentation](https://docs.rs/time/0.2.25/time/) - `time` 라이브러리의 공식 문서입니다.

## 참고

이번 글에서는 날짜를 문자열로 변환하는 간단한 예제를 다루었지만, `chrono`와 `time` 라이브러리를 활용하면 더 다양한 형식으로 날짜를 변환할 수 있습니다. 적절한 형식을 선택하여 날짜를 변환하여 개발자와 사용자 모두가 쉽게 읽고 이해할 수 있도록 하시길 바랍니다.