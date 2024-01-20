---
title:                "json 처리하기"
html_title:           "Rust: json 처리하기"
simple_title:         "json 처리하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/working-with-json.md"
---

{{< edit_this_page >}}

## 무엇이며 왜: 
JSON 작업이란 무엇인지와 왜 프로그래머들이 그것을 하는지에 대해 알아보겠습니다. 
JSON 작업은 데이터를 다루는 방법 중 하나로서 많은 프로그래머들이 사용하는 방법 중 하나입니다. 

## 사용 방법: 
Rust 코드 블록 내에 코딩 예제와 출력 예제를 보여드리겠습니다.
```Rust
extern crate serde_json; // Rust에서 JSON을 다루는 라이브러리
use serde_json::{Result, Value}; // JSON 작업을 위한 라이브러리
fn main() {
    // JSON 형식의 문자열
    let json_str = r#"
        {
            "name": "John",
            "age": 30,
            "is_married": false
        }
    "#;
    // String 타입의 JSON 데이터를 파싱
    let parsed: Value = serde_json::from_str(json_str).expect("JSON 파싱 실패");
    // name 속성 값 출력
    println!("이름: {}", parsed["name"]);
    // age 속성 값 출력
    println!("나이: {}", parsed["age"]);
    // is_married 속성 값 출력
    println!("결혼 여부: {}", parsed["is_married"]);
}
```

출력 결과: 
```
이름: John
나이: 30
결혼 여부: false
```

## 자세히 알아보기: 
JSON 작업은 현재 많은 프로그래밍 언어에서 지원하고 있는 유용한 데이터 형식입니다. 
JSON은 JavaScript Object Notation의 약자로서, 자바스크립트의 데이터 형식을 따르고 있으며, 손쉽게 다른 프로그래밍 언어에서도 사용할 수 있습니다. 
JSON을 다루는 라이브러리는 많이 존재하며, Rust에서는 serde_json 라이브러리를 사용하여 JSON 데이터를 파싱하고 다룰 수 있습니다. 
다른 대안으로는, JSON 형식을 바로 사용하지 않고 더 쉽게 다룰 수 있는 TOML 과 YAML이 있지만, JSON은 많은 프로그래머들이 익숙한 형식이기 때문에 많이 사용됩니다. 

## 관련 자료: 
- [Rust 레포지토리 전체 메뉴얼](https://doc.rust-lang.org/stable/book/)
- [JSON 홈페이지](https://www.json.org/)