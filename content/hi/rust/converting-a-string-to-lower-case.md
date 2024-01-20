---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
स्ट्रिंग को लोअर केस में परिवर्तित करना का अर्थ है कि हम उसे छोटे अक्षरों में बदल रहे हैं। यह तब किया जाता है जब हमे स्ट्रिंग का comparison या searching के दौरान case-sensitivity से बचना होता है। 

## कैसे:
Rust में, हम इंस्टेंस method `to_lowercase` का उपयोग करके यह आसानी से कर सकते हैं।

```Rust
fn main() {
    let s = "HELLO WORLD";
    println!("{}", s.to_lowercase());
}
```

यह code निम्नलिखित output उत्पन्न करेगा:
```
hello world
```

## गहराई से अध्ययन:
`to_lowercase` इंस्टेंस method हमें Unicode scalar values के साथ काम करने का समर्थन प्रदान करता है। इसका उपयोग करके, हम गैर-Latin scripts जैसे ग्रीक और सिरिलिक (मैंगीलियन भाषाओं) के characters को भी लोअरकेस में परिवर्तित कर सकते हैं।

स्ट्रिंग को लोअर केस में बदलने का वास्तविक implementation, Rust के लाइब्रेरी `std::char` में होता है। यह लाइब्रेरी `to_lowercase` method को उपयोग करती है। 

इसका एक वैकल्पिक तरीका हो सकता है - character by character iteration και lower casing of each character, लेकिन यह अधिक जटिल और time-consuming हो सकता है।

## नजर डालें:
अधिक जानकारी के लिए इन लिंकों को देखें:
- [Rust std::char Documentation](https://doc.rust-lang.org/std/char/index.html)
- [A deep dive into Unicode](https://diveintounicode.com/)