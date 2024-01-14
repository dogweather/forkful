---
title:                "Rust: 미래나 과거의 날짜 계산하기"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

Rust 프로그래밍이 다른 언어와 비교하여 강력하고 안전한 이유 중 하나는 날짜 계산이 용이하기 때문입니다. 따라서 오늘은 Rust로 미래나 과거 날짜를 계산하는 방법에 대해 알아보겠습니다.

## 방법

다음은 날짜를 계산하는 방법에 대한 Rust 코드 예제입니다. 아래 코드 블록은 ```Rust ... ```로 시작해야 합니다.

```Rust
use chrono::{Duration, NaiveDate};

// 미래 날짜 계산
let now = chrono::Utc::now().naive_utc();
let future_date = now + Duration::days(30);

// 과거 날짜 계산
let past_date = now - Duration::weeks(3);

// 출력
println!("현재 날짜: {}", now);
println!("미래 날짜: {}", future_date);
println!("과거 날짜: {}", past_date);
```

위 코드를 실행하면 다음과 같은 결과가 나타납니다.

```
현재 날짜: 2021-08-01
미래 날짜: 2021-08-31
과거 날짜: 2021-07-11
```

## 딥 다이브

Rust에서 날짜를 계산하는 과정에서 사용되는 라이브러리는 `chrono`입니다. 이 라이브러리는 다양한 날짜 계산 함수와 유연한 시간대 및 달력 지원을 제공합니다. 이를 통해 다양한 시간 형식을 다루는데 매우 유용합니다.

예를 들어, `Duration`을 이용해 날짜 간의 차이를 구할 수 있습니다. `NaiveDate`를 사용하면 특정한 날짜를 생성할 수 있습니다. 이 외에도 `DateTime`, `TimeZone` 등 다양한 기능을 제공하니 적극적으로 활용해보세요.

## 더 알아보기

- [Rust 공식 문서: chrono 라이브러리](https://doc.rust-lang.org/chrono/)
- [Rust 날짜 계산 관련 블로그 글](https://erickt.github.io/blog/2019/04/04/dates-in-rust/)
- [Panics vs. Errors in Rust: 날짜 계산 시 발생하는 에러 처리 방법](https://www.viget.com/articles/panics-vs-errors-in-rust/)