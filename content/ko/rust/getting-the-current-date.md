---
title:    "Rust: 현재 날짜 받아오기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 가져오기 위해 루스트 프로그래밍에 참여하는 이유는 무엇일까요? 현재 날짜는 우리 일상의 중요한 부분이며, 프로그램을 작성하거나 데이터를 분석할 때 필요한 정보입니다. 따라서 이를 가져와서 다양한 목적에 활용할 수 있습니다.

## 방법

현재 날짜를 가져오는 가장 간단한 방법은 스탠다드 라이브러리에 있는 `std::time::SystemTime` 구조체를 사용하는 것입니다. 다음은 이를 활용해 현재 날짜를 가져오는 예제 코드입니다.

```rust
use std::time::{SystemTime, UNIX_EPOCH};

let now = SystemTime::now();
let seconds = now.duration_since(UNIX_EPOCH).expect("Time went backwards")
               .as_secs();
println!("현재 날짜는 {}초 경과한 시간을 나타냅니다.", seconds);

```

위 코드는 `std::time::SystemTime` 구조체의 `now()` 메서드를 사용해 현재 시간을 가져옵니다. 이 시간은 `UNIX_EPOCH`와의 차이로 반환됩니다. 따라서 우리는 `as_secs()` 메서드를 사용해 초 단위로 변환하고 출력합니다.

## 깊이 파고들기

더 많은 깊이 있는 정보를 원한다면, 여러분은 `std::time::SystemTime` 구조체를 자세히 살펴볼 수 있습니다. 이 구조체는 오늘의 날짜와 시간을 나타내는 `std::time::SystemTime` 타입의 값을 생성할 수 있습니다. 또한 이를 다루는 다양한 메서드들이 제공됩니다.

## 또 다른 정보

- [Rust 공식 문서](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [Rust-lang Cookbook](https://rust-lang-nursery.github.io/rust-cookbook/datetime/dates.html)