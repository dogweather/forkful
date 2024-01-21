---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:50:30.115998-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
कंप्यूटर प्रोग्राम में रैंडम नंबर जनरेट करने का मतलब होता है ख़ास पैटर्न के बिना नंबर उत्पन्न करना। प्रोग्रामर्स इसे गेमिंग, सिक्योरिटी, साइंटिफिक सिमुलेशन, और टेस्टिंग में उपयोग करते हैं।

## How to: (कैसे करें)
रस्ट में रैंडम नंबर जनरेट करने के लिए, हमें `rand` क्रेट का उपयोग करना होता है। यहां एक उदाहरण है:

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    let random_number: u8 = rng.gen();
    println!("रैंडम नंबर: {}", random_number);
}
```

इस कोड को चलाने पर आपको हर बार एक अलग `u8` रेंज (0..255) का नंबर मिलेगा।

## Deep Dive (गहराई से जानकारी)
रैंडम नंबर जनरेटर्स (RNGs) काफी समय से हैं और एल्गोरिदमिक और हार्डवेयर RNGs में विभाजित हैं। `rand` क्रेट रस्ट में RNGs के लिए एक स्टैंडर्ड समाधान है। इसमें विभिन्न प्रकार की RNGs होती हैं, जैसे कि `thread_rng` जो कि एक क्रिप्टोग्राफिकाली सिक्योर RNG है, क्रोम सिचूएशन के अनुसार उपयुक्त हो सकता है। अनुप्रयोग की जरूरत के अनुसार हम अलग-अलग स्ट्रेटेजीज का इस्तेमाल कर सकते हैं।

## See Also (यह भी देखें)
- The Rust Programming Language Book – Using Modules to Reuse and Organize Code: [https://doc.rust-lang.org/book/ch07-02-defining-modules-to-control-scope-and-privacy.html](https://doc.rust-lang.org/book/ch07-02-defining-modules-to-control-scope-and-privacy.html)