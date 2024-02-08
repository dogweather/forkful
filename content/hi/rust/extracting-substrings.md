---
title:                "सबस्ट्रिंग्स निकालना"
aliases:
- hi/rust/extracting-substrings.md
date:                  2024-01-20T17:47:28.849441-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Substring निकालने का मतलब है एक बड़ी स्ट्रिंग से छोटा भाग (substring) प्राप्त करना। Rust में, डेवलपर्स यह इसलिए करते हैं क्योंकि कई बार केवल स्ट्रिंग के एक खास हिस्से की जरूरत होती है, जैसे यूज़र इनपुट की वैलिडेशन, पार्सिंग या फ़ाइल पाथ से फ़ाइल का नाम निकालना।

## How to: (कैसे करें:)
```Rust
fn main() {
    let my_string = "नमस्ते, Rust दुनिया!";
    let start = 9; // शुरुआत का इंडेक्स
    let end = 13; // अंत का इंडेक्स
    let substring = &my_string[start..end];
    
    println!("Extracted substring: {}", substring);
}

// सैंपल आउटपुट:
// Extracted substring: Rust
```

ध्यान दें कि Rust स्ट्रिंग्स UTF-8 encoded होती हैं, इसलिए इंडेक्स बाइट्स के आधार पर होते हैं, न कि चरित्रों के।

## Deep Dive (गहराई में जानकारी):
Rust में substring निकालने का तरीका स्मृति सुरक्षा और दक्षता पर केंद्रित है। पुरानी प्रोग्रामिंग भाषाओं में, जैसे C/C++, सबस्ट्रिंग्स हैंडल करना स्मृति सुरक्षा के दृष्टिकोण से जोखिम भरा था क्योंकि यह रन टाइम त्रुटियों का कारण बन सकता था। Rust में, स्लाइसिंग सिंटैक्स आपको सटीक बाइट इंडेक्स द्वारा स्मृति स्थान का सबसेट लेने की अनुमति देता है, जो कंपाइलर द्वारा चेक की जाती हैं। 

अल्टरनेटिव के रूप में, आप `str` मेथड `get()` या `chars().nth()` का उपयोग कर सकते हैं, जो सुरक्षित है क्योंकि ये आपको `Option` टाइप लौटाते हैं, जिसका `None` वैल्यू का मतलब है कि रिक्वेस्टेड सबस्ट्रिंग मान्य नहीं है। हालांकि, यह ज्यादा सुस्त हो सकता है क्योंकि प्रत्येक चरित्र के लिए चेक किया जाता है। 

स्लाइसिंग का उपयोग करना तेज़ होता है लेकिन बाइट बाउंडरी एरर (byte boundary error) उ‍त्पन्न हो सकते हैं यदि आप मल्टीबाइट चरित्र को गलत तरीके से काटते हैं, इसलिए सावधानी बरतें। 

## See Also (और देखें):
- Rust डाक्यूमेंटेशन में बाइटस्ट्रिंग्स पर और पढ़ें [Rust Book - Byte Strings](https://doc.rust-lang.org/book/ch08-02-strings.html#bytes-and-scalar-values-and-grapheme-clusters-oh-my).
- स्ट्रिंग स्लाइसिंग के अधिक उदाहरणों के लिए [Rust by Example](https://doc.rust-lang.org/rust-by-example/std/str.html) देखें।
- अधिक जटिल टेक्स्ट मेनिपुलेशन ऑपरेशंस के लिए `regex` क्रेट का इस्तेमाल कैसे करें यह सीखें [Rust Regex Crate](https://docs.rs/regex/1.3.9/regex/).
