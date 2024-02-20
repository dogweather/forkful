---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:50.857441-07:00
description: "Rust \u092E\u0947\u0902 JSON (JavaScript Object Notation) \u0915\u0947\
  \ \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E JSON \u0921\u0947\
  \u091F\u093E \u0915\u094B Rust \u0921\u0947\u091F\u093E \u0938\u0902\u0930\u091A\
  \u0928\u093E\u0913\u0902 \u092E\u0947\u0902 \u092A\u093E\u0930\u094D\u0938 \u0915\
  \u0930\u0928\u0947 \u0914\u0930 Rust \u0921\u0947\u091F\u093E \u0938\u0902\u0930\
  \u091A\u0928\u093E\u0913\u0902 \u0915\u094B \u0935\u093E\u092A\u0938 JSON \u092E\
  \u0947\u0902\u2026"
lastmod: 2024-02-19 22:05:10.993446
model: gpt-4-0125-preview
summary: "Rust \u092E\u0947\u0902 JSON (JavaScript Object Notation) \u0915\u0947 \u0938\
  \u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E JSON \u0921\u0947\u091F\
  \u093E \u0915\u094B Rust \u0921\u0947\u091F\u093E \u0938\u0902\u0930\u091A\u0928\
  \u093E\u0913\u0902 \u092E\u0947\u0902 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\
  \u0928\u0947 \u0914\u0930 Rust \u0921\u0947\u091F\u093E \u0938\u0902\u0930\u091A\
  \u0928\u093E\u0913\u0902 \u0915\u094B \u0935\u093E\u092A\u0938 JSON \u092E\u0947\
  \u0902\u2026"
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

Rust में JSON (JavaScript Object Notation) के साथ काम करना JSON डेटा को Rust डेटा संरचनाओं में पार्स करने और Rust डेटा संरचनाओं को वापस JSON में सीरियलाइज़ करने के बारे में है। प्रोग्रामर्स इसे वेब API, कॉन्फ़िगरेशन फ़ाइलों, या किसी भी डेटा एक्सचेंज फॉर्मेट के साथ इंटरैक्ट करने के लिए करते हैं जहां JSON इसके हल्के और मानव-पठनीय प्रारूप के कारण इस्तेमाल किया जाता है।

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
