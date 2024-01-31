---
title:                "नई परियोजना शुरू करना"
date:                  2024-01-20T18:04:23.196908-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
नया प्रोजेक्ट शुरू करते वक्त हम Rust का सरल संरचना बनाते हैं। ये इसलिए किया जाता है ताकि एक स्वच्छ और संगठित आधार पर हम कोडिंग शुरू कर सकें।

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
