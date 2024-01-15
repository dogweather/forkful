---
title:                "JSON के साथ काम करना"
html_title:           "Rust: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

रस्ट एक मौजूदा और अधिक भाषावादी भाषा है जो डीकोडिंग और एन्कोडिंग जैसे JSON डेटा को प्रसंस्करण करने के लिए शानदार है। हालांकि, अगर आप पहली बार रस्ट का उपयोग कर रहे हैं तो आपको JSON डेटा से सामने कई चुनौतियां आ सकती हैं।

## कैसे करें

{\```rust
use serde_json::{Value, json};

fn main() {
    // नए JSON वैल्यू बनाएं
    let my_object = json!({
        "name": "John",
        "age": 25,
        "favorite_fruit": "Mango"
    });

    // वैल्यू के अनुसार प्रिंट करें
    println!("{:#?}", my_object);

    // डेटा एक्सेस करें
    println!("नाम: {}", my_object["name"]);
    println!("उम्र: {}", my_object["age"]);
}

{\```

{\```bash
OUTPUT:
{
    "name": "John",
    "age": 25,
    "favorite_fruit": "Mango"
}
नाम: John
उम्र: 25
{\```}

## गहराई में जाएं

जब आप JSON डेटा को प्रसंस्करण करते हैं, तो आपको सुनिश्चित करना होगा कि आपका JSON डेटा वैध है और उसमें कोई अनुमत विलंब नहीं है। सरल एफपीआई (API) के साथ काम करने के लिए, आपको विशेषकर JSON को सीरियलाइज और डीसीरियलाइज करना आवश्यक हो सकता है। आप सर्वोत्तम तरीके से यह करने के लिए serde अनुप्रयोग का उपयोग कर सकते हैं।

## और जानें

यदि आप रस्ट में JSON डेटा के साथ काम करने के बारे में अधिक जानना चाहते हैं, तो इन लिंकों पर जाएं:

- [serde डॉक्यूमेंटेशन](https://docs.rs/serde_json/1.0.65/serde_json/)
- [Jaspy का रस्ट और JSON विकास](https://jaspy.dev/rust-and-json/)
- [रस्ट में सीरियलाइज और डीसीरियलाइज करने का एक नमूना कोड](https://blog.logrocket.com/serializing-and-deserializing-json-in-rust-with-serde/)