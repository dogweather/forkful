---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:50.857441-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Rust \u092E\u0947\
  \u0902 JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F, \u0938\u0940\u0930\u093F\u092F\u0932\u093E\
  \u0907\u091C\u093C\u0947\u0936\u0928 \u0914\u0930 \u0921\u0940\u0938\u0940\u0930\
  \u093F\u092F\u0932\u093E\u0907\u091C\u093C\u0947\u0936\u0928 \u0915\u0947 \u0932\
  \u093F\u090F `serde` \u0915\u094D\u0930\u0947\u091F \u0915\u0947 \u0938\u093E\u0925\
  -\u0938\u093E\u0925 `serde_json` \u0915\u093E \u0935\u094D\u092F\u093E\u092A\u0915\
  \ \u0930\u0942\u092A \u0938\u0947 \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932\
  \u2026"
lastmod: '2024-03-13T22:44:51.999880-06:00'
model: gpt-4-0125-preview
summary: "Rust \u092E\u0947\u0902 JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\
  \u092E \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, \u0938\u0940\u0930\
  \u093F\u092F\u0932\u093E\u0907\u091C\u093C\u0947\u0936\u0928 \u0914\u0930 \u0921\
  \u0940\u0938\u0940\u0930\u093F\u092F\u0932\u093E\u0907\u091C\u093C\u0947\u0936\u0928\
  \ \u0915\u0947 \u0932\u093F\u090F `serde` \u0915\u094D\u0930\u0947\u091F \u0915\u0947\
  \ \u0938\u093E\u0925-\u0938\u093E\u0925 `serde_json` \u0915\u093E \u0935\u094D\u092F\
  \u093E\u092A\u0915 \u0930\u0942\u092A \u0938\u0947 \u0907\u0938\u094D\u0924\u0947\
  \u092E\u093E\u0932 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948\
  \u0964 \u0938\u092C\u0938\u0947 \u092A\u0939\u0932\u0947, \u0907\u0928\u094D\u0939\
  \u0947\u0902 \u0905\u092A\u0928\u0940 `Cargo.toml` \u092E\u0947\u0902 \u0936\u093E\
  \u092E\u093F\u0932 \u0915\u0930\u0928\u093E \u0938\u0941\u0928\u093F\u0936\u094D\
  \u091A\u093F\u0924 \u0915\u0930\u0947\u0902."
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

## कैसे करें:
Rust में JSON के साथ काम करने के लिए, सीरियलाइज़ेशन और डीसीरियलाइज़ेशन के लिए `serde` क्रेट के साथ-साथ `serde_json` का व्यापक रूप से इस्तेमाल किया जाता है। सबसे पहले, इन्हें अपनी `Cargo.toml` में शामिल करना सुनिश्चित करें:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### उदाहरण 1: JSON को एक Rust Struct में डीसीरियलाइज करें
एक Rust struct निर्धारित करें और `Deserialize` और `Serialize` के लिए derive मैक्रो का इस्तेमाल करें:

```rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
    email: String,
}

fn main() {
    let json_data = r#"
        {
            "id": 1,
            "name": "Jane Doe",
            "email": "jane.doe@example.com"
        }
    "#;

    let user: User = serde_json::from_str(json_data).unwrap();

    println!("User ID: {}", user.id);
    println!("User Name: {}", user.name);
    println!("User Email: {}", user.email);
}
```

**आउटपुट:**

```
User ID: 1
User Name: Jane Doe
User Email: jane.doe@example.com
```

### उदाहरण 2: एक Rust Struct को JSON में सीरियलाइज़ करें
वही `User` struct का इस्तेमाल करते हुए:

```rust
let user = User {
    id: 1,
    name: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let json_data = serde_json::to_string(&user).unwrap();

println!("{}", json_data);
```

**आउटपुट:**

```json
{"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}
```

ये उदाहरण JSON को Rust संरचनाओं में डीसीरियलाइज़ करने और Rust संरचनाओं को वापस JSON स्ट्रिंग्स में सीरियलाइज़ करने के मूल प्रवाह का प्रदर्शन करते हैं। Serde JSON के साथ काम करने के लिए वैकल्पिक फील्ड्स, जटिल नेस्टिंग, और JSON द्वारा सीधे समर्थित नहीं किये गए प्रकारों सहित, एक समृद्ध टूलसेट प्रदान करता है।
