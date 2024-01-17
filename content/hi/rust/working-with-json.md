---
title:                "Json के साथ काम करना"
html_title:           "Rust: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/working-with-json.md"
---

{{< edit_this_page >}}

# JSON क्या है और इसे क्यों काम करते हैं? 

JSON (Javascript Object Notation) एक पॉपुलर डेटा फॉर्मेट है जो स्ट्रिंग, नंबर्स, बूलियन, विकल्प, एरे और ऑब्जेक्ट्स को संगठन और संचित करने के लिए इस्तेमाल किया जाता है। यह डेटा फॉर्मेट प्रोग्रामर्स को अपने प्रोग्राम से डेटा पढ़ने और उसे संशोधित करने की आसानी प्रदान करता है।

## कैसे करें: 

जब हम अपने रस्ट कोड में JSON डेटा को पार्स करना चाहते हैं, हम पहले serde_json पैकेज को अपनी डिपेंडेंसी में एड करते हैं। फिर हम डेटा को एक स्ट्रिंग में लोड करते हैं, उसे serde_json::from_str() फंक्शन के साथ डेकोड करते हैं और अपने डेकोड किए गए डेटा को उपयोग करते हैं। नीचे दिए गए उदाहरण में, हम एक JSON स्ट्रिंग को डेकोड करके उसमें दिए गए विकल्पों के मान को प्रिंट करते हैं। 

```Rust
use serde_json;

fn main() {
    let data = r#"{"name":"John", "age":30, "is_programmer":true}"#;
    let decoded: serde_json::Value = serde_json::from_str(data).unwrap();
    
    let name = decoded["name"].as_str().unwrap();
    let age = decoded["age"].as_u64().unwrap();
    let is_programmer = decoded["is_programmer"].as_bool().unwrap();
    
    println!("Name: {}", name);
    println!("Age: {}", age);
    if is_programmer {
        println!("Is a programmer? Yes");
    } else {
        println!("Is a programmer? No");
    }
}
```

आउटपुट: 
```
Name: John
Age: 30
Is a programmer? Yes
```

## गहराई में जाएं: 

JSON का इतिहास 1999 में डब्ल्यूएसडब्ल्यू कोलेन ने बनाया था। यह डेटा फॉर्मेट आसानी से समझने और लिखने के लिए छोटे से टेक्स्ट फाइलों पर आधारित था। कुछ अल्टरनेटिव्स हाल ही में आए हैं, जगह-जगह जो XML, बीएमपी, और टेक्स्ट फाइलों जैसे दूसरे डेटा फॉर्मेट प्रदान करते हैं। रस्ट में, हम इन डेटा फाइलों को पार्स करने के लिए serde पैकेज का भी इस्तेमाल कर सकते हैं। इसके अलावा, हम अपने अनुप्रयोग के आवश्यकतानुसार अपना सेरिलाइज़न और डिसिरियलाइज़ेशन भी कर सकते हैं। 

## और जानें: 

1. [Rust डॉक्यूमेंटेशन](https://www.rust-lang.org/learn) - रस्ट के ऑफिशियल डॉक्यूमेंटेशन के साथ सुरुवात करने के लिए यहां देखें। 
2. [json.org](https://www.json.org/json-en.html) - जेसन का क्यों-कैसे और उसके डिज़ाइन पर अधिक जानने के लिए आधिकारिक साइट पर जाएं। 
3. [serde_json डॉक्यूमेंटेशन](https://docs.rs/serde_json/