---
title:                "TOML के साथ काम करना"
aliases: - /hi/rust/working-with-toml.md
date:                  2024-01-26T04:27:05.166876-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML एक मानव-पठनीय डेटा सीरियलाइजेशन भाषा है, जिसे अक्सर कॉन्फ़िग्स के लिए इस्तेमाल किया जाता है। प्रोग्रामर TOML का उपयोग इसकी सादगी और स्पष्टता के लिए करते हैं, जो रस्ट में हैश मैप में आसानी से अनुवाद करती है।

## कैसे करें:
```Rust
// 1. अपने Cargo.toml में 'toml' क्रेट शामिल करें
// [निर्भरताएँ]
// toml = "0.5"

// 2. Rust में एक संरचना में TOML को डीसीरियलाइज़ करें
use toml::Value;

fn main() {
    let toml_content = r#"
        [server]
        host = "localhost"
        port = 8080
    "#;

    let value = toml_content.parse::<Value>().unwrap();
    let host = value.get("server").unwrap().get("host").unwrap();
    let port = value.get("server").unwrap().get("port").unwrap();
    
    println!("सर्वर {}:{} पर चल रहा है", host, port);
    // आउटपुट: सर्वर "localhost":8080 पर चल रहा है
}
```

## गहराई में जानकारी
TOML, जिसका पूरा नाम Tom's Obvious, Minimal Language है, 2013 में टॉम प्रेस्टन-वर्नर द्वारा बनाई गई थी। इसका उद्देश्य कॉन्फ़िग फाइलों के लिए JSON या YAML से अधिक पठनीय होना है। TOML के डिजाइन पर अस्पष्टता रहित सिंटैक्स, न्यूनतावाद, और डेटा प्रकारों के लिए आसान मैपिंग पर ध्यान केंद्रित किया गया है।

TOML के विकल्पों में JSON, YAML, और XML शामिल हैं, लेकिन उन परिदृश्यों में TOML जीतता है जहाँ मानव पठनीयता और गैर-प्रोग्रामरों द्वारा फाइल संपादन महत्वपूर्ण है। रस्ट में TOML के साथ काम करते समय, serde सीरियलाइज़ेशन और डीसीरियलाइज़ेशन के लिए मजबूत नींव प्रदान करता है, रस्ट के संरचनाओं पर TOML को बिना किसी परेशानी के मैप करने के लिए लक्षणों का उपयोग करता है।

TOML के साथ काम करते समय एक चुनौती इसकी प्रकारों और संरचना पर सख्ती है। प्रोग्रामर को रस्ट में TOML का प्रभावी ढंग से उपयोग करने के लिए TOML डेटा की स्कीमा को परिलक्षित करने वाली एक अच्छी संरचित रस्ट प्रकार प्रणाली परिभाषित करनी चाहिए।

## देखें भी
- [TOML प्रलेखन](https://toml.io/en/)
- [serde_toml क्रेट](https://docs.rs/serde_toml/)
- [रस्ट प्रोग्रामिंग भाषा पुस्तक](https://doc.rust-lang.org/stable/book/)
- [TOML GitHub रिपो](https://github.com/toml-lang/toml)
