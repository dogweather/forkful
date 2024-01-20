---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
문자열에서 날짜를 파싱한다는 것은, 정해진 포맷의 문자열을 날짜 형식으로 바꾸는 것을 뜻합니다. 프로그래머들이 이를 사용하는 이유는, 사용자 입력, 로그 파일 등에서 날짜 정보를 추출하고 이를 활용하기 편리하기 때문입니다.

## 이렇게 하세요:
최신 버전의 Rust를 사용하여, 문자열에서 날짜를 어떻게 파싱하는지 확인해 보겠습니다. 그러기 위해 `chrono` 라이브러리를 사용합니다.

```Rust
use chrono::NaiveDate;
    
let s = "2021-07-06";
let dt = NaiveDate::parse_from_str(s, "%Y-%m-%d");

match dt {
    Ok(date) => println!("{}", date),
    Err(e) => println!("Error parsing date: {:?}", e),
}
```
위 코드 실행시 결과는 다음과 같습니다:

```Rust
2021-07-06
```

## 디테일:
문자열에서 날짜를 파싱하는 데 사용되는 기술은 오래전부터 있었습니다. 언어마다 다양한 방법이 존재하며, Rust에서는 `chrono`라는 외부 라이브러리를 사용하여 직관적이고 간단하게 날짜 파싱을 수행할 수 있습니다.

날짜 파싱의 구현 메커니즘은 비교적 단순합니다. 문자열이 주어지면, 그 문자열이 정해진 날짜 포맷에 있는 숫자와 문자들의 위치에 따라 분석됩니다. 위치에 따라 연도, 월, 일으로 나누어져 각각의 값이 반환됩니다.

## 참고 자료:
Rust로 날짜 파싱을 하기 위한 자세한 정보는 아래 링크에서 찾아볼 수 있습니다. 

- chrono 라이브러리: https://docs.rs/chrono/0.4.11/chrono/
- Rust 날짜 및 시간에 대한 학습 자료: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html