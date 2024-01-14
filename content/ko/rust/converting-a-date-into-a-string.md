---
title:    "Rust: 날짜를 문자열로 변환하는 방법"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 작업은 프로그래밍에서 매우 자주 사용됩니다. Rust 언어에서 이 작업을 어떻게 수행하는지 배워보세요. 

## 방법

이 작업을 수행하기 위해서는 Rust 언어에서 제공하는 `chrono` 라이브러리를 사용해야 합니다. 먼저, 해당 라이브러리를 프로젝트에 추가해야 합니다. 

```rust
use chrono::{DateTime, Utc}; 
```

다음으로, 원하는 날짜와 시간을 `DateTime` 타입으로 만들어야 합니다. 

```rust
let now: DateTime<Utc> = Utc::now();
```

`DateTime` 타입을 사용하면, 여러 가지 메소드를 활용하여 날짜를 원하는 형식으로 변환할 수 있습니다. 예를 들어, `format()` 메소드를 사용하면 날짜를 커스텀한 형식으로 변환할 수 있습니다.

```rust
let now_string = now.format("%Y-%m-%d").to_string();
```

위의 코드를 실행하면, 현재 날짜가 `2021-03-10` 와 같은 형식으로 변환됩니다. 또는 `to_rfc28226()` 메소드를 사용하면 인터넷 표준 형식에 맞게 변환할 수도 있습니다.

```rust
let now_string = now.to_rfc2822();
```

각 메소드의 반환값은 `&str` 형식이기 때문에, 원한다면 `to_string()` 메소드를 사용하여 `String` 타입으로 변환할 수도 있습니다.

## 깊이있게 알아보기

`chrono` 라이브러리는 날짜 및 시간을 처리하기 위한 다양한 기능을 제공합니다. `DateTime` 타입 뿐만 아니라 `TimeZone`, `Duration`, `Offset` 등 다양한 타입들을 활용하여 원하는 날짜 및 시간을 다양한 형식으로 변환할 수 있습니다. 또한, 해당 라이브러리는 다양한 예외 상황에 대응하기 위한 메소드들도 제공하기 때문에, 안정적인 프로그래밍을 위해 항상 예외처리를 고려해야 합니다.

## 관련 자료

- [chrono 라이브러리 문서](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust 공식 홈페이지](https://www.rust-lang.org/ko)
- [Rust로 날짜 및 시간 다루기](https://steemit.com/kr/@gogo2415/rust)