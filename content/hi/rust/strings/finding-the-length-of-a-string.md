---
date: 2024-01-20 17:48:18.322179-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0906\u0909\
  \u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T21:53:53.949116-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0906\u0909\u091F\u092A\
  \u0941\u091F."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u094D\u091E\u093E\u0924 \u0915\u0930\u0928\u093E"
weight: 7
---

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
