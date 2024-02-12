---
title:                "स्ट्रिंग को कैपिटलाइज करना"
date:                  2024-02-03T19:07:32.294251-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Rust में एक स्ट्रिंग को कैपिटलाइज़ करना इसे संशोधित करने की प्रक्रिया है ताकि इसका पहला अक्षर, यदि वह अक्षर हो, तो बड़ा (अपरकेस) हो, जबकि बाकी स्ट्रिंग अपरिवर्तित रहे। प्रोग्रामर अक्सर शीर्षक के लिए शब्दों को तैयार करने या उपयोगकर्ता इनपुट में संगति सुनिश्चित करने जैसे स्वरूपण उद्देश्यों के लिए इस कार्रवाई को करते हैं।

## कैसे:

Rust में एक स्ट्रिंग को कैपिटलाइज़ करने के लिए, आपके पास दो प्रमुख रास्ते हैं: मानक पुस्तकालय की सुविधाओं का उपयोग करना या अधिक जटिल या विशिष्ट आवश्यकताओं के लिए तृतीय-पक्ष क्रेट्स का उपयोग करना। यहाँ आप दोनों को कैसे अंजाम दे सकते हैं।

### Rust की मानक पुस्तकालय का इस्तेमाल करते हुए

Rust की मानक पुस्तकालय स्ट्रिंगों को कैपिटलाइज़ करने का सीधा तरीका प्रदान नहीं करती, लेकिन आप स्ट्रिंग के अक्षरों को मैनिपुलेट करके इसे प्राप्त कर सकते हैं।

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hello";
    println!("{}", capitalize_first(my_string)); // आउटपुट: Hello
}
```

### `heck` क्रेट का इस्तेमाल करते हुए

एक अधिक सरल दृष्टिकोण के लिए, खासकर जब आप एक बड़े पाठ संसाधन संदर्भ में काम कर रहे हों, तो आप तृतीय-पक्ष पुस्तकालयों जैसे `heck` का उपयोग करना पसंद कर सकते हैं। `heck` क्रेट विभिन्न मामला परिवर्तन कार्यक्षमताओं को प्रदान करता है, जिसमें स्ट्रिंग्स को कैपिटलाइज़ करने का एक सरल तरीका शामिल है।

पहले, अपने `Cargo.toml` में `heck` जोड़ें:

```toml
[dependencies]
heck = "0.4.0"
```

फिर, इसका उपयोग अपनी स्ट्रिंग को कैपिटलाइज़ करने के लिए करें:

```rust
extern crate heck; // Rust 2018 संस्करण या बाद में आवश्यक नहीं
use heck::TitleCase;

fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // आउटपुट: Hello World
}
```

नोट: `heck` द्वारा प्रदान की गई `to_title_case` विधि स्ट्रिंग में प्रत्येक शब्द को कैपिटलाइज़ करती है, जो कि आप जो खोज रहे हैं उससे अधिक हो सकती है यदि आप केवल स्ट्रिंग के पहले अक्षर को कैपिटलाइज़ करना चाहते हैं। अपनी विशिष्ट आवश्यकताओं के अनुसार अपने उपयोग को समायोजित करें।