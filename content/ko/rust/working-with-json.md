---
title:                "Rust: json 작업하기"
simple_title:         "json 작업하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/working-with-json.md"
---

{{< edit_this_page >}}

## 왜
JSON을 다루는 작업에 참여하는 이유는 무엇일까요? 이는 데이터를 효율적으로 저장하고 전송할 수 있는 가벼운 형식이기 때문입니다. 또한 다양한 프로그래밍 언어에서 모두 지원되기 때문에 유연하게 쓸 수 있습니다.

## 해야할 일
먼저 JSON을 사용할 수 있도록 라이브러리를 추가해야 합니다. 그런 다음 다음과 같이 쉽게 JSON을 다룰 수 있습니다:

```Rust
use serde_json::{Result, Value};

fn main() -> Result<()> {
    // Sample JSON object
    let data = r#"
        {
          "name": "John Doe",
          "age": 30,
          "city": "Seoul"
        }"#;

    // Parsing JSON string into Value type
    let v: Value = serde_json::from_str(data)?;

    // Accessing data from JSON object
    println!("Name: {}", v["name"]); // Output: John Doe
    println!("Age: {}", v["age"]); // Output: 30
    println!("City: {}", v["city"]); // Output: Seoul

    Ok(())
}
```
위의 예제에서는 `serde_json` 라이브러리를 사용하여 JSON 형식의 데이터를 다루는 방법을 보여줍니다. `from_str()` 함수를 사용하여 JSON 문자열을 Value 타입으로 변환하고, 배열과 마찬가지로 인덱스를 사용하여 데이터에 접근할 수 있습니다.

## 깊은 탐구
JSON은 매우 유연한 형식이지만 가끔 복잡한 데이터를 다룰 때 어려움을 겪을 수 있습니다. 이 경우 `serde_json` 라이브러리의 `to_string()` 메서드를 사용하여 데이터를 JSON 형식으로 다시 변환하고, `get()` 메서드를 사용하여 원하는 데이터에 접근할 수 있습니다. 또한 복잡한 데이터 구조를 위해 `serde`의 매우 강력한 기능인 직렬화와 역직렬화를 사용할 수 있습니다.

## See Also
- [Official Serde documentation](https://docs.rs/serde_json/1.0.62/serde_json/)
- [JSON tutorial by w3schools](https://www.w3schools.com/js/js_json_intro.asp)
- [Learning Rust: Working with JSON](https://learning-rust.github.io/docs/a6.bonus.md.html#working-with-json)