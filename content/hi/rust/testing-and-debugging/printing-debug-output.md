---
date: 2024-01-20 17:53:34.037793-07:00
description: "Debug output \u092A\u094D\u0930\u093F\u0902\u091F \u0915\u0930\u0928\
  \u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u093F \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E \u0915\u093E \u0921\u0947\u091F\u093E \u0915\u0902\
  \u0938\u094B\u0932 \u092A\u0930 \u0926\u093F\u0916\u093E\u0928\u093E, \u092F\u0939\
  \ \u091C\u093E\u0928\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u093F \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u0915\u0948\u0938\u0947 \u091A\
  \u0932 \u0930\u0939\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930\u094D\u0938 \u092C\u0917\u094D\u0938 \u0915\u0940 \u092A\
  \u0939\u091A\u093E\u0928 \u0915\u0930\u0928\u0947 \u0914\u0930 \u0915\u094B\u0921\
  \u2026"
lastmod: 2024-02-19 22:05:10.958984
model: gpt-4-1106-preview
summary: "Debug output \u092A\u094D\u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E\
  \ \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u093F \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E \u0915\u093E \u0921\u0947\u091F\u093E \u0915\u0902\u0938\
  \u094B\u0932 \u092A\u0930 \u0926\u093F\u0916\u093E\u0928\u093E, \u092F\u0939 \u091C\
  \u093E\u0928\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u093F \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E \u0915\u0948\u0938\u0947 \u091A\u0932\
  \ \u0930\u0939\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\
  \u093E\u092E\u0930\u094D\u0938 \u092C\u0917\u094D\u0938 \u0915\u0940 \u092A\u0939\
  \u091A\u093E\u0928 \u0915\u0930\u0928\u0947 \u0914\u0930 \u0915\u094B\u0921\u2026"
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Debug output प्रिंट करना मतलब है कि प्रोग्राम का डेटा कंसोल पर दिखाना, यह जानने के लिए कि प्रोग्राम कैसे चल रहा है। प्रोग्रामर्स बग्स की पहचान करने और कोड को समझने के लिए यह करते हैं।

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
