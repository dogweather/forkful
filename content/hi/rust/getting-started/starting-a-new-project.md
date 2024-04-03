---
date: 2024-01-20 18:04:23.196908-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) ."
lastmod: '2024-03-13T22:44:51.962767-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0928\u0908 \u092A\u0930\u093F\u092F\u094B\u091C\u0928\u093E \u0936\u0941\
  \u0930\u0942 \u0915\u0930\u0928\u093E"
weight: 1
---

## कैसे करें? (How to:)
```Rust
// Rust में नया प्रोजेक्ट बनाने के लिए निम्न कमांड उपयोग में लाएं:

cargo new my_project

/* 
आउटपुट:
     Created binary (application) `my_project` package
*/

// अब `my_project` डायरेक्टरी में जाएं और सरल हेलो वर्ल्ड प्रोग्राम चलाएं:

cd my_project
cargo run

/* 
आउटपुट:
   Compiling my_project v0.1.0 (/path/to/my_project)
    Finished dev [unoptimized + debuginfo] target(s) in 0.5 secs
     Running `target/debug/my_project`
Hello, world!
*/
```

## विस्तृत जानकारी (Deep Dive)
Rust प्रोग्रामिंग भाषा का विकास 2010 में Mozilla Research द्वारा शुरू हुआ था। `cargo new` कमांड Rust के package manager और build system, Cargo का उपयोग करके एक नया प्रोजेक्ट बनाने के लिए है। इससे आपको मानक फाइल संरचना और `Cargo.toml` कॉन्फ़िगरेशन फाइल मिलती है जो निर्भरताओं और build settings को संभालती है। विकल्पों के रूप में, आप लाइब्रेरी या बाइनरी प्रोजेक्ट तैयार कर सकते हैं और git repository भी आरंभ कर सकते हैं।

## सम्बंधित स्रोत (See Also)
- Rust के आधिकारिक दस्तावेज़ [यहाँ](https://www.rust-lang.org/learn) देखें।
- Cargo के बारे में और पढ़ें [यहाँ](https://doc.rust-lang.org/cargo/)।
- Rust के लिए प्रारंभिक गाइड प्राप्त करें [यहाँ](https://www.rust-lang.org/learn/get-started)।
