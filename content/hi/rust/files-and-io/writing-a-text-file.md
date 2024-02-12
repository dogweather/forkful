---
title:                "एक टेक्स्ट फ़ाइल लिखना"
aliases:
- /hi/rust/writing-a-text-file/
date:                  2024-02-03T19:30:26.214044-07:00
model:                 gpt-4-0125-preview
simple_title:         "एक टेक्स्ट फ़ाइल लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
रस्ट में एक पाठ फाइल लिखना फाइल सिस्टम पर एक फाइल बनाने, उसमें लिखने और संभवतः डेटा जोड़ने की प्रक्रिया शामिल है। प्रोग्रामर इस ऑपरेशन को डेटा को स्थायी रूप से सहेजने के लिए करते हैं, जैसे की एप्लिकेशन लॉग्स, कॉन्फ़िगरेशन, या उपयोगकर्ता-निर्मित सामग्री, जिससे कार्यक्रम निष्पादन के दायरे से परे डेटा की दीर्घायु सुनिश्चित होती है।

## कैसे करें:
रस्ट की मानक पुस्तकालय फ़ाइल मैनिपुलेशन के लिए रोबस्ट उपकरण प्रदान करती है, जो मुख्य रूप से `std::fs` और `std::io` मॉड्यूल्स के अंदर निहित है। यहाँ एक पाठ फाइल बनाने और उसमें लिखने का एक बेसिक उदाहरण दिया गया है:

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::create("hello.txt")?;
    file.write_all(b"Hello, world!")?;
    Ok(())
}
```

इस कोड को चलाने के बाद, आपको "Hello, world!" की सामग्री वाली `hello.txt` नाम की एक फाइल मिलेगी।

अधिक जटिल परिदृश्यों के लिए, जैसे कि एक फाइल में जोड़ना या बड़े डेटा को कुशलतापूर्वक संभालना, रस्ट अतिरिक्त कार्यक्षमता प्रदान करता है। यहां बताया गया है कि किसी मौजूदा फाइल में पाठ कैसे जोड़ें:

```rust
use std::fs::OpenOptions;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .open("hello.txt")?;
        
    file.write_all(b" Adding more text.")?;
    Ok(())
}
```

इसे चलाने पर `hello.txt` के अंत में " Adding more text." जोड़ देगा।

कुछ मामलों में, तीसरे पक्ष के पुस्तकालयों का उपयोग करना फ़ाइल ऑपरेशन्स को सरल बना सकता है। उदाहरण के लिए, `serde` क्रेट, `serde_json` के साथ मिलकर, JSON फॉर्मेट में डेटा संरचनाओं को सिरीयलाइज़ और डिसिरीयलाइज़ करने की अनुमति देता है, जो फाइलों को लिखने के लिए एक उच्च-स्तरीय दृष्टिकोण प्रदान करता है:

```rust
use serde::{Serialize, Deserialize};
use serde_json;
use std::fs::File;

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
}

fn main() -> std::io::Result<()> {
    let user = User { id: 1, name: "Jane Doe".into() };
    let file = File::create("user.json")?;
    serde_json::to_writer(file, &user)?;
    Ok(())
}
```

उपरोक्त कोड चलाने के बाद, `user.json` में `User` संरचना का एक JSON प्रतिनिधित्व होगा। ध्यान दें कि `serde` और `serde_json` का उपयोग करने के लिए इन क्रेट्स को आपकी `Cargo.toml` में जोड़ना होगा।

मानक पुस्तकालय के माध्यम से या बाहरी क्रेट्स की मदद से रस्ट में पाठ फाइलों को लिखना, आपके अनुप्रयोगों में डेटा परिपालन का प्रबंधन करने का एक सरल फिर भी शक्तिशाली तरीका है।
