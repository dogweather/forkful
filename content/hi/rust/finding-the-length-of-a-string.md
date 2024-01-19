---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Rust में स्ट्रिंग की लम्बाई कैसे निकालें? 

## क्या और क्यों?

स्ट्रिंग की लम्बाई निकालना मतलब होता है कि उस स्ट्रिंग में कितने अक्षर हैं। यह तब जरुरी होता है जब प्रोग्रामर को विशेष उद्देश्यों के लिए उस स्ट्रिंग के आकार की जानकारी की आवश्यकता होती है, जैसे कि मेमोरी management या string manipulation.

## कैसे करें:

```Rust
// स्ट्रिंग स्थापित करें
let s = "नमस्ते, दुनिया!";
// लम्बाई निकालें और मुद्रित करें
println!("{}", s.len());
```

यह कोड, स्ट्रिंग की लम्बाई को इन्टेजर में बदलकर दिखाएगा।

## गहरी जानकारी

1. ऐतिहासिक प्रसंग: Rust में स्ट्रिंग की लम्बाई निकालने के लिए पहले `str::len` method का उपयोग किया जाता था। 
2. विकल्प: `s.len()` सही है, लेकिन यह Unicode को सही ढंग से हैंडल नहीं करता। Unicode के साथ काम करने के लिए, आप `s.chars().count()` का उपयोग कर सकते हैं।
3. विवरण: `len` method `String` के बारे में जानकारी प्रदान करता है, जो बाइटों की संख्या होती है, न कि Unicode अक्षरों की संख्या। 

## संबंधित सूत्र

1. Rust डॉक्यूमेंटेशन: [String Docs](https://doc.rust-lang.org/std/string/struct.String.html)
2. Stack Overflow: [How to get string length in Rust?](https://stackoverflow.com/questions/26946646/rust-function-to-find-the-length-of-a-string)
3. Rust Language ब्लॉग: [Strings in Rust](https://blog.rust-lang.org/2015/05/11/traits.html)