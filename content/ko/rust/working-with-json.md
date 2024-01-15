---
title:                "JSON 작업하기"
html_title:           "Rust: JSON 작업하기"
simple_title:         "JSON 작업하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/working-with-json.md"
---

{{< edit_this_page >}}

## 왜 이것을 해야하나요?

JSON은 현대 웹 프로그래밍에서 널리 사용되는 데이터 형식 중 하나입니다. 이것을 사용하여 데이터를 교환하고 저장하는 데 매우 유용하며, 많은 언어에서 지원되고 있습니다. 따라서 JSON을 잘 다루는 것은 프로그래밍에서 필수적인 기술 중 하나입니다.

## 이렇게 하세요!

### JSON 데이터 생성하기

```Rust
extern crate serde_json;

use serde_json::{Value};
use serde_json::json;

fn main() {
    // Key-value 쌍으로 이루어진 Rust HashMap 정의
    let mut user = HashMap::new();
    user.insert("name", "John");
    user.insert("age", "30");
    
    // JSON 데이터로 변환
    let json_data = json!(user);
    println!("JSON 데이터:\n {}", json_data);
}
```

```
## 출력:
{
    "name": "John",
    "age": "30"
}
```

### JSON 데이터 읽기

```Rust
extern crate serde_json;

use serde_json::Value;

fn main() {
    // JSON 데이터 읽기
    let json_data = r#"{
        "name": "John",
        "age": "30"
    }"#;

    // JSON을 파싱하여 Value 타입 객체로 변환
    let user: Value = serde_json::from_str(json_data).unwrap();

    // Value 타입 객체에서 원하는 값 추출
    let name = user["name"].as_str().unwrap();
    let age = user["age"].as_str().unwrap();

    println!("이름: {}", name);
    println!("나이: {}", age);
}
```

```
## 출력:
이름: John
나이: 30
```

## 더 깊게 알아보기

### Rust의 serde_json 라이브러리

Rust는 serde_json 라이브러리를 통해 JSON 데이터를 쉽게 다룰 수 있도록 지원하고 있습니다. 이 라이브러리를 사용하면 JSON 데이터를 Rust 데이터 타입으로 쉽게 변환하고, 반대로 Rust 데이터를 JSON 형식으로 쉽게 만들 수 있습니다. 또한 이 라이브러리는 JSON을 보다 효율적으로 파싱할 수 있는 기능을 제공하여 성능을 향상시킵니다.

## 참고 자료

- [Rust 공식 문서 - serde_json](https://docs.serde.rs/serde_json/)
- [Rust는 실패하지 않는다 - Serde와 JSON으로 작업하기](https://kimsunwoong.github.io/rust-programming/serde_json/)