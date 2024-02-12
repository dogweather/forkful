---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
date:                  2024-01-20T17:48:18.322179-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
एक स्ट्रिंग की लंबाई जानना मतलब यह पता करना कि स्ट्रिंग में कितने characters हैं। यह जानकारी हमें लूप्स, स्ट्रिंग मैनिपुलेशन, और वेलिडेशन चेक्स के समय काफी काम आती है।

## How to: (कैसे करें:)
```rust
fn main() {
    let greeting = "नमस्ते";
    let length = greeting.chars().count();

    println!("स्ट्रिंग की लंबाई: {}", length);
}
```
आउटपुट:
```
स्ट्रिंग की लंबाई: 6
```

## Deep Dive (गहराई से जानकारी)
रस्ट में, `len()` मेथड बाइट्स की संख्या देता है, न कि characters की। इसलिए, यूनिकोड स्ट्रिंग्स का सही लंबाई पाने के लिए `chars().count()` का प्रयोग किया जाता है। यह सही तरीके से यूनिकोड स्कैलर वैल्यूज को गिनता है। 

`len()` मेथड तब काम आता है जब आपको बाइट्स की सटीक संख्या जाननी हो, जैसे कि रॉ बाइनरी डेटा प्रोसेस करते वक्त। याद रखें, `len()` से मिलने वाली संख्या characters की वास्तविक संख्या नहीं हो सकती है अगर आपके स्ट्रिंग में विशेष characters या इमोजी शामिल हों।

स्ट्रिंग में ग्राफीम क्लस्टर्स के लंबाई को मापने के लिए, आपको बाहरी क्रेट्स (libraries) का उपयोग करना पड़ सकता है, क्योंकि रस्ट का स्टैंडर्ड लाइब्रेरी डायरेक्टली इसे सपोर्ट नहीं करती।

## See Also (और जानकारी)
- [Rust Book - Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust Documentation on std::string::String](https://doc.rust-lang.org/std/string/struct.String.html)
- [Unicode Scalar Values](https://unicode.org/glossary/#unicode_scalar_value)
