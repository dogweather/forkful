---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:16.333102-07:00
description: "\u0930\u0947\u0917\u094D\u092F\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\
  \u092A\u094D\u0930\u0947\u0936\u0928, \u092F\u093E \u0930\u0947\u091C\u0947\u0915\
  \u094D\u0938, \u0921\u0947\u0935\u0932\u092A\u0930\u094B\u0902 \u0915\u094B \u0909\
  \u0928\u094D\u0928\u0924 \u092A\u0948\u091F\u0930\u094D\u0928-\u092E\u0948\u091A\
  \u093F\u0902\u0917 \u0924\u0915\u0928\u0940\u0915\u094B\u0902 \u0915\u0947 \u0938\
  \u093E\u0925 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\
  \u094B \u0916\u094B\u091C\u0928\u0947, \u092E\u093F\u0932\u093E\u0928 \u0915\u0930\
  \u0928\u0947 \u0914\u0930 \u092E\u0948\u0928\u0940\u092A\u0941\u0932\u0947\u091F\
  \ \u0915\u0930\u0928\u0947 \u0915\u0940 \u0905\u0928\u0941\u092E\u0924\u093F \u0926\
  \u0947\u0924\u0947 \u0939\u0948\u0902\u0964 Rust\u2026"
lastmod: '2024-03-13T22:44:51.944241-06:00'
model: gpt-4-0125-preview
summary: "\u0930\u0947\u0917\u094D\u092F\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\
  \u092A\u094D\u0930\u0947\u0936\u0928, \u092F\u093E \u0930\u0947\u091C\u0947\u0915\
  \u094D\u0938, \u0921\u0947\u0935\u0932\u092A\u0930\u094B\u0902 \u0915\u094B \u0909\
  \u0928\u094D\u0928\u0924 \u092A\u0948\u091F\u0930\u094D\u0928-\u092E\u0948\u091A\
  \u093F\u0902\u0917 \u0924\u0915\u0928\u0940\u0915\u094B\u0902 \u0915\u0947 \u0938\
  \u093E\u0925 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\
  \u094B \u0916\u094B\u091C\u0928\u0947, \u092E\u093F\u0932\u093E\u0928 \u0915\u0930\
  \u0928\u0947 \u0914\u0930 \u092E\u0948\u0928\u0940\u092A\u0941\u0932\u0947\u091F\
  \ \u0915\u0930\u0928\u0947 \u0915\u0940 \u0905\u0928\u0941\u092E\u0924\u093F \u0926\
  \u0947\u0924\u0947 \u0939\u0948\u0902\u0964 Rust\u2026"
title: "\u0930\u0947\u0917\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\
  \u0947\u0936\u0928\u094D\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

रेग्युलर एक्सप्रेशन, या रेजेक्स, डेवलपरों को उन्नत पैटर्न-मैचिंग तकनीकों के साथ स्ट्रिंग्स को खोजने, मिलान करने और मैनीपुलेट करने की अनुमति देते हैं। Rust में, रेजेक्स का उपयोग करने से टेक्स्ट डाटा को कुशलतापूर्वक पार्स और हैंडल करने में मदद मिलती है, जिससे डाटा वैलिडेशन, खोज और टेक्स्ट ट्रांसफॉर्मेशन जैसे कार्य अधिक सरलीकृत और बनाए रखने योग्य हो जाते हैं।

## कैसे करें:

रस्ट की `regex` लाइब्रेरी रेग्युलर एक्सप्रेशन्स के साथ काम करने के लिए एक जाने-माने समाधान है। इसका उपयोग करने के लिए, आपको पहले इसे अपने `Cargo.toml` में जोड़ना होगा:

```toml
[dependencies]
regex = "1"
```

फिर, आप अपने Rust कोड में रेजेक्स कार्यक्षमताओं को लागू कर सकते हैं। यहाँ कुछ सामान्य ऑपरेशंस को करने का तरीका बताया गया है:

### एक स्ट्रिंग में पैटर्न की मिलान

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-15";

    println!("क्या टेक्स्ट डेट पैटर्न से मेल खाता है? {}", re.is_match(date));
    // उत्पादन: क्या टेक्स्ट डेट पैटर्न से मेल खाता है? सच
}
```

### मैचों का पता लगाना और उन्हें एक्सेस करना

```rust
use regex::Regex;

fn main() {
    let text = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(text) {
        println!("भाषा: {}, वर्ष: {}", &cap[1], &cap[2]);
    }
    // उत्पादन:
    // भाषा: Rust, वर्ष: 2023
    // भाषा: C++, वर्ष: 2022
    // भाषा: Python, वर्ष: 2021
}
```

### टेक्स्ट बदलना

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let text = "Rust 2023, C++ 2022, Python 2021";
    let replaced = re.replace_all(text, "$1 को $2 में अपडेट किया गया");

    println!("अपडेटेड टेक्स्ट: {}", replaced);
    // उत्पादन: अपडेटेड टेक्स्ट: Rust को 2023 में अपडेट किया गया, C++ को 2022 में अपडेट किया गया, Python को 2021 में अपडेट किया गया
}
```

### एक रेजेक्स का उपयोग करके टेक्स्ट विभाजित करना

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // किसी भी गैर-शब्द वर्ण पर विभाजित
    let text = "Rust-C++-Python-Go";

    let fields: Vec<&str> = re.split(text).collect();

    for field in fields {
        println!("भाषा: {}", field);
    }
    // उत्पादन:
    // भाषा: Rust
    // भाषा: C++
    // भाषा: Python
    // भाषा: Go
}
```

रस्ट में रेग्युलर एक्सप्रेशन के साथ शुरुआत करने के लिए ये उदाहरण एक बुनियादी मार्गदर्शिका प्रदान करते हैं। जैसे-जैसे आपकी आवश्यकताएं अधिक जटिल होती जाती हैं, `regex` क्रेट जटिल पैटर्न मिलान और टेक्स्ट मैनिपुलेशन कार्यों के लिए एक धन की कार्यक्षमता प्रदान करता है।
