---
title:                "JSON 다루기"
date:                  2024-01-19
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON은 데이터를 저장하고 전송할 때 사용하는 텍스트 기반의 가벼운 데이터 교환 형식이에요. 프로그래머들은 구조가 단순하고 읽기 쉬워서 서버와 클라이언트 간 통신이나 설정 파일에서 자주 쓰고 있죠.

## How to:
JSON 다루기 위해 `serde`와 `serde_json` 크레이트를 사용해봅시다. 다음 예시는 JSON 값을 읽고, 쓰고, 변환하는 기본적인 방법을 보여드릴 거예요.

```Rust
extern crate serde;
extern crate serde_json;

use serde::{Serialize, Deserialize};

// 단순한 구조체를 정의합니다.
#[derive(Serialize, Deserialize, Debug)]
struct Person {
    name: String,
    age: u8,
    is_programmer: bool,
}

fn main() {
    // JSON 문자열
    let data = r#"
        {
            "name": "홍길동",
            "age": 25,
            "is_programmer": true
        }"#;
    
    // JSON을 구조체로 변환
    let p: Person = serde_json::from_str(data).unwrap();
    println!("이름: {}", p.name);

    // 구조체를 JSON 문자열로 변환
    let serialized = serde_json::to_string(&p).unwrap();
    println!("{}", serialized);
}
```

샘플 출력:
```
이름: 홍길동
{"name":"홍길동","age":25,"is_programmer":true}
```

## Deep Dive
JSON은 JavaScript Object Notation의 약자로, 2001년에 더글라스 크록포드에 의해 소개되었습니다. XML이나 YAML 같은 대안이 있지만, JSON은 더 간결하고 파싱이 빠르다는 장점이 있어요. Rust에서 `serde`는 직렬화(Serialization)/역직렬화(Deserialization)를 위한 에코시스템을 제공하며, `serde_json`은 이를 JSON 데이터 포맷에 특화시킨 크레이트입니다.

## See Also
- [Serde 공식 웹사이트](https://serde.rs/)
- [JSON 공식 사이트](https://www.json.org/json-en.html)
