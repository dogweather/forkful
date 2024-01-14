---
title:                "Rust: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것이 왜 중요한지 궁금하신가요? 이 글에서 러스트 프로그래밍 언어로 날짜를 표현하고 문자열로 변환하는 방법을 알아보세요. 또한 이 과정에서 러스트의 특징과 장점도 살펴볼 수 있을 거예요.

## 방법

날짜를 문자열로 변환하는 방법은 간단해요. 우선 `chrono` 라이브러리를 불러온 뒤, `DateTime` 구조체를 사용해서 날짜를 정의해줍니다. 그리고 `format` 메소드를 사용하여 문자열로 변환할 수 있어요.

```Rust
use chrono::{DateTime, Utc, Datelike};

// 현재 날짜를 정의합니다.
let now: DateTime<Utc> = Utc::now();

// 문자열로 변환합니다.
let string_date = now.format("%Y-%m-%d").to_string();

println!("날짜: {}", string_date);
```

출력 결과는 다음과 같이 나올 거예요:

```
날짜: 2021-12-03
```

포매팅 문자열을 자유롭게 변경하여 원하는 형식으로 날짜를 표현할 수 있습니다. `format` 메소드의 자세한 사용법은 [chrono 문서](https://docs.rs/chrono/)를 참고하세요.

## 딥 다이브

위 코드에서 사용한 `DateTime` 구조체는 러스트의 표준 라이브러리에 포함되어 있지 않지만, `chrono` 라이브러리를 통해 사용할 수 있습니다. 이 라이브러리는 날짜와 시간을 다루는 많은 기능들을 제공합니다.

또한 러스트의 특징 중 하나인 속도와 안전성을 잘 보여주는 예제라고 할 수 있습니다. 무거운 작업을 수행하는 코드에서도 빠르게 동작하며 메모리를 안전하게 관리합니다.

## 추가 공부할 만한 링크들

- [chrono 문서](https://docs.rs/chrono/)
- [러스트 공식 홈페이지](https://www.rust-lang.org/ko)
- [러스트 커뮤니티 포럼](https://www.rust-lang.org/ko/community)
- [러스트 뉴스레터](https://this-week-in-rust.org/)