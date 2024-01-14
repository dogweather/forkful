---
title:    "Rust: 미래나 과거의 날짜 계산하기"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# 왜

날짜를 미래나 과거로 계산하는 것에 관심이 있는 사람들은, 그 날짜가 중요하거나 의미 있는 이벤트나 기념일과 관련되어 있기 때문일 수 있습니다.

## 어떻게 하나요?

간단한 날짜 계산을 위해서는 먼저 Rust의 날짜 및 시간 관련 라이브러리를 임포트해야 합니다. 그 후에는 다음과 같은 코드 블록을 사용하여 원하는 날짜를 계산할 수 있습니다.

```
use chrono::{DateTime, Duration, NaiveDateTime, Utc};

fn main() {
  // 오늘 날짜
  let today = Utc::today();

  // 10일 후 날짜 계산
  let future_date = today + Duration::days(10);

  println!("Future Date: {}", future_date);
}
```

위 코드를 실행하면 다음과 같은 결과가 나타납니다.

```
Future Date: 2021-05-01
```

이 외에도 시간이나 분, 초 단위로도 계산이 가능합니다. 또한 범위가 큰 시간간의 계산을 위해서는 `Duration::seconds()`, `Duration::minutes()` 등의 메서드를 사용할 수 있습니다.

## 깊게 파고들기

날짜 계산을 위한 Rust의 표준 라이브러리인 `chrono`는 매우 풍부한 기능을 제공합니다. 예를 들어, 특정 날짜를 기준으로 다음 달 3일 후 날짜를 계산하거나, 주말을 제외한 다음 주 일요일 날짜를 계산하는 것도 가능합니다.

또한 업무에서 많이 사용되는 `Timezone`과 관련된 기능도 제공합니다. 예를 들어, 다른 지역의 시간을 현재 지역 시간으로 변환하는 등의 작업이 가능합니다.

날짜와 시간 관련 작업은 매우 중요하고 복잡하기 때문에, `chrono` 라이브러리의 공식 문서를 읽어보는 것을 추천합니다.

## 더 알아보기

- [Rust `chrono` 라이브러리 공식 문서](https://docs.rs/chrono/0.4.19/chrono/)