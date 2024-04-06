---
date: 2024-01-20 17:40:16.008692-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Rust \u0915\
  \u0940 `String` type \u092E\u0947\u0902 `.to_lowercase()` \u092E\u0947\u0925\u0921\
  \ UTF-8 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0915\u0947 \u0938\u093E\u0925\
  \ \u0905\u091A\u094D\u091B\u0947 \u0938\u0947 \u0915\u093E\u092E \u0915\u0930\u0924\
  \u093E \u0939\u0948\u0964 \u092F\u0947 Unicode \u0915\u0947 case mappings \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u093E\u2026"
lastmod: '2024-04-05T22:51:06.613226-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Rust \u0915\u0940 `String`\
  \ type \u092E\u0947\u0902 `.to_lowercase()` \u092E\u0947\u0925\u0921 UTF-8 \u091F\
  \u0947\u0915\u094D\u0938\u094D\u091F \u0915\u0947 \u0938\u093E\u0925 \u0905\u091A\
  \u094D\u091B\u0947 \u0938\u0947 \u0915\u093E\u092E \u0915\u0930\u0924\u093E \u0939\
  \u0948\u0964 \u092F\u0947 Unicode \u0915\u0947 case mappings \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u093E \u0939\u0948, \u091C\u093F\u0938\
  \u0947 \u0907\u0902\u091F\u0930\u0928\u0947\u0936\u0928\u0932 \u0915\u092E\u094D\
  \u092F\u0941\u0928\u093F\u091F\u0940 \u0928\u0947 \u092E\u093E\u0928\u094D\u092F\
  \u0924\u093E \u0926\u0940 \u0939\u0948\u0964 `.to_lowercase()` \u0926\u0941\u0928\
  \u093F\u092F\u093E\u092D\u0930 \u0915\u0940 \u092D\u093E\u0937\u093E\u0913\u0902\
  \ \u092E\u0947\u0902 \u0935\u093F\u0938\u094D\u0924\u093E\u0930 \u0938\u0947 \u0932\
  \u093E\u0917\u0942 \u0939\u094B\u0924\u093E \u0939\u0948, \u091C\u093F\u0938\u092E\
  \u0947\u0902 \u0915\u0908 special case conditions \u0936\u093E\u092E\u093F\u0932\
  \ \u0939\u0948\u0902\u0964 \u0907\u0938\u0915\u0947 alternatives \u092E\u0947\u0902\
  \ `to_ascii_lowercase()` \u092D\u0940 \u0939\u0948, \u091C\u094B \u0915\u0947\u0935\
  \u0932 ASCII \u091A\u0930\u093F\u0924\u094D\u0930 \u0915\u0947 \u0932\u093F\u090F\
  \ \u0915\u093E\u092E \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092F\u0926\u093F\
  \ \u0906\u092A\u0915\u0947 text \u092E\u0947\u0902 \u0915\u0947\u0935\u0932 ASCII\
  \ \u091A\u0930\u093F\u0924\u094D\u0930 \u0939\u094B\u0902, \u0924\u094B \u092F\u0939\
  \ \u0924\u0947\u091C\u093C \u0914\u0930 \u0938\u0930\u0932 \u0938\u092E\u093E\u0927\
  \u093E\u0928 \u0939\u094B \u0938\u0915\u0924\u093E \u0939\u0948\u0964 Rust \u0915\
  \u093E `.to_lowercase()` \u0909\u091A\u094D\u091A performance \u0914\u0930 safety\
  \ \u0915\u0947 \u0938\u093E\u0925 \u0921\u093F\u091C\u093E\u0907\u0928 \u0915\u093F\
  \u092F\u093E \u0917\u092F\u093E \u0939\u0948\u0964 \u092F\u0939 \u0939\u092E\u0947\
  \u0936\u093E \u090F\u0915 \u0928\u0908 `String` object \u092C\u0928\u093E\u0924\u093E\
  \ \u0939\u0948 \u0915\u094D\u092F\u094B\u0902\u0915\u093F Rust \u092E\u0947\u0902\
  \ strings immutable \u0939\u094B\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
weight: 4
---

## How to: (कैसे करें:)
```Rust
fn main() {
    let greeting = "नमस्ते, दुनिया!";
    let lower_greeting = greeting.to_lowercase();
    println!("Original: {}", greeting);
    println!("Lowercase: {}", lower_greeting);
}
```
Output:
```
Original: नमस्ते, दुनिया!
Lowercase: नमस्ते, दुनिया!
```
ध्यान दें कि हिंदी चरित्र पहले से ही एक निचले-केस फॉर्म में हैं, तो परिवर्तन अदृश्य होगा। अंग्रेजी उदाहरण के लिए:

```Rust
fn main() {
    let english_greeting = "Hello, World!";
    let lower_english_greeting = english_greeting.to_lowercase();
    println!("Original: {}", english_greeting);
    println!("Lowercase: {}", lower_english_greeting);
}
```
Output:
```
Original: Hello, World!
Lowercase: hello, world!
```

## Deep Dive (गहराई से जानकारी)
Rust की `String` type में `.to_lowercase()` मेथड UTF-8 टेक्स्ट के साथ अच्छे से काम करता है। ये Unicode के case mappings का उपयोग करता है, जिसे इंटरनेशनल कम्युनिटी ने मान्यता दी है।

`.to_lowercase()` दुनियाभर की भाषाओं में विस्तार से लागू होता है, जिसमें कई special case conditions शामिल हैं। 

इसके alternatives में `to_ascii_lowercase()` भी है, जो केवल ASCII चरित्र के लिए काम करता है। यदि आपके text में केवल ASCII चरित्र हों, तो यह तेज़ और सरल समाधान हो सकता है।

Rust का `.to_lowercase()` उच्च performance और safety के साथ डिजाइन किया गया है। यह हमेशा एक नई `String` object बनाता है क्योंकि Rust में strings immutable होते हैं।

## See Also (और जानकारी के लिए)
- Rust के official documentation में `.to_lowercase()` पर [डिटेल जानकारी](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)।
- [Unicode case mappings](http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt) के बारे में और जानें।
- Rust by Example पर एक [tutorial](https://doc.rust-lang.org/stable/rust-by-example/std/str.html) है जो strings के अन्य operations को समझाती है।
- [`Ownership`](https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html) और [`Borrowing`](https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html) के सिद्धांत Rust में memory safety की चाबी हैं, जो `.to_lowercase()` जैसे operations को संभव बनाते हैं।
