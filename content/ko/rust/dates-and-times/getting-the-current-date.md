---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:04.237732-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Rust\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uB294 \uD604\uC7AC \uC2DC\uAC04\uC744 \uC5BB\uB294 \uB370\uB294 \uC81C\
  \uD55C\uC801\uC774\uC9C0\uB9CC \uBE60\uB978 \uBC29\uBC95\uC744 \uC81C\uACF5\uD558\
  \uC9C0\uB9CC, \uC9C1\uC811\uC801\uC73C\uB85C \uCE98\uB9B0\uB354 \uD615\uC2DD\uC758\
  \ \uD604\uC7AC \uB0A0\uC9DC\uB97C \uC5BB\uB294 \uAC83\uC740 \uC544\uB2D9\uB2C8\uB2E4\
  . \uB2E4\uC74C\uC740 \uADF8 \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.931095-06:00'
model: gpt-4-0125-preview
summary: "Rust\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB294 \uD604\uC7AC\
  \ \uC2DC\uAC04\uC744 \uC5BB\uB294 \uB370\uB294 \uC81C\uD55C\uC801\uC774\uC9C0\uB9CC\
  \ \uBE60\uB978 \uBC29\uBC95\uC744 \uC81C\uACF5\uD558\uC9C0\uB9CC, \uC9C1\uC811\uC801\
  \uC73C\uB85C \uCE98\uB9B0\uB354 \uD615\uC2DD\uC758 \uD604\uC7AC \uB0A0\uC9DC\uB97C\
  \ \uC5BB\uB294 \uAC83\uC740 \uC544\uB2D9\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

## 사용 방법:


### Rust의 표준 라이브러리 사용하기
Rust의 표준 라이브러리는 현재 시간을 얻는 데는 제한적이지만 빠른 방법을 제공하지만, 직접적으로 캘린더 형식의 현재 날짜를 얻는 것은 아닙니다. 다음은 그 방법입니다:

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("현재 시간: Unix Epoch 이후 {}초.", n.as_secs()),
        Err(_) => panic!("SystemTime이 Unix Epoch 이전입니다!"),
    }
}
```

출력:
```
현재 시간: Unix Epoch 이후 1615390665초.
```

### Chrono 라이브러리 사용하기
더욱 포괄적인 날짜 및 시간 기능을 포함하여 현재 날짜를 얻으려면 `chrono` 라이브러리를 사용해야 합니다. 먼저, `chrono`를 `Cargo.toml`에 추가하세요:

```toml
[dependencies]
chrono = "0.4"
```

그런 다음, `chrono`를 사용하여 현재 날짜를 얻습니다:

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let now = Local::now();
    println!("현재 날짜: {}-{}-{}", now.year(), now.month(), now.day());
}
```

출력:
```
현재 날짜: 2023-4-20
```

`chrono` 라이브러리는 날짜와 시간을 다루기 쉽게 만들어 주며, 현재 날짜를 검색하는 것을 넘어서 날짜와 시간에 대한 파싱, 포맷팅, 산술 연산 등 폭넓은 기능을 제공합니다.
