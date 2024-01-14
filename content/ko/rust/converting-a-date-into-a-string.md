---
title:                "Rust: 날짜를 문자열로 변환하기"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜
날짜를 문자열로 변환하는 것에 참여하는 이유는 효율적인 프로그래밍을 위해서입니다. 날짜는 많은 프로그램에서 자주 사용되기 때문에 문자열로 변환하기 위한 기술은 중요합니다.

## 다루는 방법
다음은 날짜를 문자열로 변환하는 방법을 보여주는 코드 예제입니다. 이 예제는 Rust 언어를 기준으로 작성되었습니다. 

```Rust
use chrono::{Utc, format::strftime};
 
// 현재 날짜를 가져옵니다.
let now = Utc::now();
 
// 날짜를 원하는 형식의 문자열로 변환합니다.
let date_string = now.format("%Y-%m-%d").to_string();
 
println!("오늘의 날짜는 {}입니다.", date_string);
```

위의 코드 예제를 실행하면 다음과 같은 결과가 출력됩니다.

```
오늘의 날짜는 2021-11-01입니다.
```

## 깊이 파헤치기
날짜를 문자열로 변환하는 방법은 다양한 형태로 존재합니다. 위의 예제에서는 `format()` 메소드를 사용하여 원하는 형식의 문자열을 생성하는 방법을 보여주었습니다. 그러나 Rust에서는 `serde`라는 라이브러리를 사용하여 날짜를 다양한 형식으로 직렬화할 수도 있습니다. 또한 `chrono` 라이브러리에서 제공하는 다양한 포맷 지정자를 사용하여 날짜를 원하는 형태로 변환할 수 있습니다.

## 관련 자료
- [Rust Language 공식 문서](https://doc.rust-lang.org/book/ch03-02-data-types.html#structs)
- [Serde 라이브러리 문서](https://serde.rs/)
- [Chrono 라이브러리 문서](https://docs.rs/chrono/0.4.19/chrono/)