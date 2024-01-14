---
title:                "Rust: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

컴퓨터 프로그래밍을 배우는 많은 사람들은 시간과 날짜를 처리하는 법에 대해 배우게 됩니다. 그 중에서도 Rust는 에러 처리와 보안 등 여러 가지 측면에서 매우 효율적인 언어로써, 날짜와 시간을 다루는 기능 역시 강력합니다. 그래서 오늘은 우리가 Rust에서 현재 날짜를 가져오는 방법에 대해 알아보겠습니다.

## 사용 방법

Rust에서 현재 날짜를 가져오는 가장 간단한 방법은 `chrono` 라이브러리를 사용하는 것입니다. 우선 `Cargo.toml` 파일에 다음과 같이 `chrono`를 추가해줍니다.

```
[dependencies]
chrono = "0.4.19"
```

그리고 아래와 같이 코드를 작성해줍니다.

```
use chrono::{DateTime, Utc};

fn main() {
  let now: DateTime<Utc> = Utc::now();
  println!("현재 날짜와 시간: {}", now);
}
```

이 코드를 실행하면 다음과 같은 결과가 출력됩니다.

```
현재 날짜와 시간: 2021-09-28 11:30:00 UTC
```

## 심층 분석

`chrono` 라이브러리는 날짜와 시간을 다루는 크래프트 수 있는 기능을 제공합니다. 위 코드에서는 `Utc`를 사용하여 현재 UTC 시간을 가져왔지만, `Local`을 사용하면 현재 로컬 시간을 가져올 수 있습니다. 또한 `DateTime` 타입은 `format()` 메서드를 사용하여 원하는 형식으로 날짜와 시간을 포맷팅할 수 있습니다.

## 더 알아보기

- [Rust Programming Language Official Website](https://www.rust-lang.org/ko)
- [Rust Cookbook - Date and Time](https://rust-lang-nursery.github.io/rust-cookbook/datetime.html)
- [Chrono Official Documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust Crash Course - Date and Time](https://www.youtube.com/watch?v=EuYcG340C3M)

## 참고하기

- [Markdown-Cheatsheet](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet)
- [Rust 튜토리얼 블로그](https://www.rust-lang.org/ko/learn)
- [Rust Korea 커뮤니티 페이지](https://www.rust-lang.org/ko/community)
- [How to Rust 동영상 시리즈](https://www.youtube.com/playlist?list=PLJbE2Yu2zumDD5vy2BuSHvFZU0a6RDmgb)