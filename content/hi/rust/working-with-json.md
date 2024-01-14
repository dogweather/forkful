---
title:                "Rust: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

JSON, यानि जयेसन (JavaScript Object Notation), एक popular data format है जो की modern web programming में extensively use किया जाता है। इसका format readable और easy to parse होने के कारण, यह developers के बीच काफी popular है। इसीलिए, Rust में JSON के साथ काम करना बहुत important है और होना भी चाहिए।

## कैसे करे

जब आप Rust में JSON का प्रभावी तरीके से use करते हैं, तो आप अपने code को बहुत organized और readable बना सकते हैं। पहले, आपको Rust में `serde` और `serde_json` libraries को install करना होगा। फिर, आप `use serde::{Deserialize, Serialize};` को import करके start कर सकते हैं। आप `#[derive(Deserialize, Serialize)]` attribute का use करे करके अपने structs को JSON compatible बना सकते हैं। आप नीचे दी गई code snippet को चेक कर सकते हैं।

```Rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct User {
    name: String,
    age: u8,
    active: bool,
}

fn main() {
    let user = User {
        name: String::from("John"),
        age: 25,
        active: true,
    };

    let json = serde_json::to_string(&user).unwrap();
    println!("{}", json);
}
```

और output आपको निम्न दिखाई देगा:

```Rust
{"name":"John","age":25,"active":true}
```

## गहराई में

जब आप Rust में JSON के साथ काम करते हैं, तो आप पाएंगे की यह आसान है। लेकिन, आपको JSON की समझ की गहराई में जानने की आवश्यकता होगी। आपको इसके अलावा भी serialization और deserialization के बारे में समझना होगा। आप उन्हें customize कर सकते हैं ताकि आप अपने data को specific formats में convert कर सके। आप दी गई links को checkout करके और Rust में JSON के साथ काम करना सीखके और पूरी ताक लेंगे।

## देखें भी

- [Official Rust docs for working with JSON](https://doc.rust-lang.org/beta/book/ch08-02-strings.html)
- [Rust for JavaScript Developers: Handling JSON](https://www.sitepoint.com/rust-javascript-stack-handling-json/)
- [Simple JSON Serialization and Deserialization from Scratch in Rust](https://kylewbanks.com/blog/simple-json-serialization-and-deserialization-in-rust)