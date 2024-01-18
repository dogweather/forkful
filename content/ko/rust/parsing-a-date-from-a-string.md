---
title:                "문자열에서 날짜 추출하기"
html_title:           "Rust: 문자열에서 날짜 추출하기"
simple_title:         "문자열에서 날짜 추출하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

날짜를 문자열에서 추출하는 것을 파싱한다고 합니다. 프로그래머들이 이를 하는 이유는, 일반적으로 다양한 형식의 날짜 데이터를 다룰 때 필요하기 때문입니다.

## 방법:

Rust에서는 `chrono` 라이브러리를 사용하여 날짜를 문자열에서 추출할 수 있습니다. 아래는 간단한 예시 코드와 출력 예시입니다.
```Rust
use chrono::prelude::*;
let date_str = "2021-06-12";
let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d").unwrap();
println!("년: {}, 월: {}, 일: {}", date.year(), date.month(), date.day());
```

출력:
```sh
년: 2021, 월: 6, 일: 12
```

## 깊이 파고들기:

날짜 파싱은 날짜 포맷이 다양한 언어나 시스템에서 일관된 형태로 다루기 위해 필요합니다. 예를 들어, 한국어에서는 년-월-일 순서로 표현되지만, 미국에서는 월-일-년 순서로 표현됩니다. Rust에서는 `chrono` 라이브러리 외에도 `regex` 라이브러리를 사용하여 정규식을 이용한 날짜 파싱도 가능합니다.

## 관련 자료:

- [Rust 공식 문서 - chrono 라이브러리](https://doc.rust-lang.org/chrono/)
- [Rust 공식 문서 - regex 라이브러리](https://doc.rust-lang.org/regex/)