---
date: 2024-01-20 18:04:23.196908-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) Rust \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u092D\u093E\u0937\
  \u093E \u0915\u093E \u0935\u093F\u0915\u093E\u0938 2010 \u092E\u0947\u0902 Mozilla\
  \ Research \u0926\u094D\u0935\u093E\u0930\u093E \u0936\u0941\u0930\u0942 \u0939\u0941\
  \u0906 \u0925\u093E\u0964 `cargo new` \u0915\u092E\u093E\u0902\u0921 Rust \u0915\
  \u0947 package manager \u0914\u0930 build\u2026"
lastmod: '2024-04-05T22:51:06.633531-06:00'
model: gpt-4-1106-preview
summary: ") Rust \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\
  \u0917 \u092D\u093E\u0937\u093E \u0915\u093E \u0935\u093F\u0915\u093E\u0938 2010\
  \ \u092E\u0947\u0902 Mozilla Research \u0926\u094D\u0935\u093E\u0930\u093E \u0936\
  \u0941\u0930\u0942 \u0939\u0941\u0906 \u0925\u093E\u0964 `cargo new` \u0915\u092E\
  \u093E\u0902\u0921 Rust \u0915\u0947 package manager \u0914\u0930 build system,\
  \ Cargo \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u090F\
  \u0915 \u0928\u092F\u093E \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\u091F\
  \ \u092C\u0928\u093E\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0939\u0948\u0964\
  \ \u0907\u0938\u0938\u0947 \u0906\u092A\u0915\u094B \u092E\u093E\u0928\u0915 \u092B\
  \u093E\u0907\u0932 \u0938\u0902\u0930\u091A\u0928\u093E \u0914\u0930 `Cargo.toml`\
  \ \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\u0930\u0947\u0936\u0928 \u092B\
  \u093E\u0907\u0932 \u092E\u093F\u0932\u0924\u0940 \u0939\u0948 \u091C\u094B \u0928\
  \u093F\u0930\u094D\u092D\u0930\u0924\u093E\u0913\u0902 \u0914\u0930 build settings\
  \ \u0915\u094B \u0938\u0902\u092D\u093E\u0932\u0924\u0940 \u0939\u0948\u0964 \u0935\
  \u093F\u0915\u0932\u094D\u092A\u094B\u0902 \u0915\u0947 \u0930\u0942\u092A \u092E\
  \u0947\u0902, \u0906\u092A \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940\
  \ \u092F\u093E \u092C\u093E\u0907\u0928\u0930\u0940 \u092A\u094D\u0930\u094B\u091C\
  \u0947\u0915\u094D\u091F \u0924\u0948\u092F\u093E\u0930 \u0915\u0930 \u0938\u0915\
  \u0924\u0947 \u0939\u0948\u0902 \u0914\u0930 git repository \u092D\u0940 \u0906\u0930\
  \u0902\u092D \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964."
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
