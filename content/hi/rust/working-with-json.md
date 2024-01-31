---
title:                "JSON के साथ काम करना"
date:                  2024-01-19
simple_title:         "JSON के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Rust में JSON के साथ काम करने का मतलब है स्ट्रक्चर्ड डेटा को पढ़ना, लिखना और संशोधित करना। यह वेब APIs से डेटा साझा करने या कॉन्फ़िगरेशन फाइल्स के रूप में इस्तेमाल करने के लिए किया जाता है।

## How to: (कैसे करें:)
Rust में `serde` क्रेट इस्तेमाल करके JSON से काम करें:

```rust
// dependencies in Cargo.toml
// serde = "1.0"
// serde_json = "1.0"

use serde::{Deserialize, Serialize};
use serde_json::{Result, Value};

#[derive(Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
    phones: Vec<String>,
}

fn main() -> Result<()> {
    // JSON String
    let data = r#"
        {
            "name": "John Doe",
            "age": 30,
            "phones": ["+44 1234567", "+44 2345678"]
        }"#;

    // Deserialize JSON to Rust Struct
    let p: Person = serde_json::from_str(data)?;

    // Use `p` as a Rust object
    println!("Please call {} at the number {}", p.name, p.phones[0]);

    // Serialize Rust Struct to JSON
    let serialized = serde_json::to_string(&p)?;

    // Print out the serialized JSON string
    println!("Serialized Person : {}", serialized);

    Ok(())
}
```
आउटपुट:
```
Please call John Doe at the number +44 1234567
Serialized Person : {"name":"John Doe","age":30,"phones":["+44 1234567","+44 2345678"]}
```

## Deep Dive (गहराई से जानकारी)
JSON (JavaScript Object Notation) पहली बार JavaScript के लिए डेटा फॉर्मेट के रूप में उभरा, पर अब यह भाषा-निरपेक्ष है। Rust में `serde` और `serde_json` क्रेट्स सीरियलाइजेशन और डीसीरियलाइजेशन के लिए मानक माने जाते हैं। XML और YAML जैसे अन्य फॉर्मेट भी हैं, पर JSON इसकी सादगी और पढ़ने में आसानी के कारण ज्यादा लोकप्रिय है। `serde` JSON के साथ काम करते समय custom serialization या complex data types को हैंडल करने के लिए उपयोगी है।

## See Also (और देखें)
- Serde Official Documentation: [https://serde.rs/](https://serde.rs/)
- Serde JSON API Documentation: [https://docs.serde.rs/serde_json/](https://docs.serde.rs/serde_json/)
- Rust Programming Language Book Chapter on Serialization: [https://doc.rust-lang.org/book/ch20-00-final-project-a-web-server.html#storing-json-in-the-filesystem](https://doc.rust-lang.org/book/ch20-00-final-project-a-web-server.html#storing-json-in-the-filesystem)
