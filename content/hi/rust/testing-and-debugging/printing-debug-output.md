---
date: 2024-01-20 17:53:34.037793-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Rust \u092E\
  \u0947\u0902 debug information \u0915\u094B \u092A\u094D\u0930\u093F\u0902\u091F\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `println!` \u092E\u0948\
  \u0915\u094D\u0930\u094B \u0914\u0930 debug trait `{:?}` \u0915\u093E \u0907\u0938\
  \u094D\u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0939\u0948\u0964\
  ."
lastmod: '2024-03-13T22:44:51.965977-06:00'
model: gpt-4-1106-preview
summary: "Rust \u092E\u0947\u0902 debug information \u0915\u094B \u092A\u094D\u0930\
  \u093F\u0902\u091F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `println!`\
  \ \u092E\u0948\u0915\u094D\u0930\u094B \u0914\u0930 debug trait `{:?}` \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0939\
  \u0948\u0964."
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

## How to: (कैसे करें:)
Rust में debug information को प्रिंट करने के लिए `println!` मैक्रो और debug trait `{:?}` का इस्तेमाल होता है।

```Rust
fn main() {
    let number = 42;
    println!("Debug output: {:?}", number);
}
```
उपरोक्त कोड का आउटपुट होगा:
```
Debug output: 42
```

अगर हमें एक स्ट्रक्चर को प्रिंट करना हो, तो Derive attribute का इस्तेमाल करते हैं:

```Rust
#[derive(Debug)]
struct Student {
    name: String,
    grade: char,
}

fn main() {
    let student = Student {
        name: String::from("Rahul"),
        grade: 'A',
    };
    println!("Student info: {:?}", student);
}
```
इसका आउटपुट कुछ इस तरह होगा:
```
Student info: Student { name: "Rahul", grade: 'A' }
```

## Deep Dive (गहराई में जानकारी)
Rust में `Debug` trait का परिचय डेवेलपर्स को उनके डेटा को आराम से प्रिंट करने देने के लिए हुआ था। इसे std::fmt मॉड्यूल में परिभाषित किया गया है। कुछ डेटा टाइप्स में `Debug` trait पहले से इम्प्लिमेंट हो चुका है, लेकिन यूजर डिफाइंड टाइप्स के लिए `#[derive(Debug)]` जोड़ना पड़ता है।

`Debug` के बजाय `Display` trait का भी इस्तेमाल हो सकता है, जो कि यूज़र-फ्रेंडली आउटपुट देता है, लेकिन उसे मैन्युअली इम्प्लिमेंट करना पड़ता है। `{:?}` की जगह `{}` का इस्तेमाल करके हम `Display` के आउटपुट को प्रिंट कर सकते हैं।

प्रिंटिंग के लिए अन्य टूल्स और लाइब्रेरीज भी हैं जैसे `log` क्रेट, जो उच्च स्तरीय लॉगिंग अभियान प्रदान करता है। लेकिन, सिंपल डीबगिंग के लिए `println!` और `eprintln!` (जो कि STDERR पर प्रिंट करता है) सबसे आम हैं।

## See Also (और देखें)
- [Rust Documentation for std::fmt](https://doc.rust-lang.org/std/fmt/)
- [Rust by Example - Debug Trait](https://doc.rust-lang.org/rust-by-example/hello/print/print_debug.html)
- [The Rust Programming Language book - Display](https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html)
