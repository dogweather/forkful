---
title:                "Yaml के साथ काम करना"
html_title:           "Rust: Yaml के साथ काम करना"
simple_title:         "Yaml के साथ काम करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

यैमल का उपयोग करके प्रोग्रामिंग करने की वजह आमतौर पर कॉन्फिगरेशन फ़ाइलों या डेटा स्ट्रक्चर्स को पढ़ने और लिखने के लिए होता है। यह एक स्वचालित और आसान तरीके से डेटा को फॉर्मेट करता है और योग्यता देखता है कि कितने संदर्भों को परिवर्तित किया गया है।

## कैसे

यैमल को रश कोड का उपयोग करके आप किसी भी डेटा संरचना को आसानी से एक्सेस और मैनिपुलेट कर सकते हैं। निम्नलिखित कोड ब्लॉक आपको इसका उपयोग कैसे करना है और एक उदाहरण देखने के लिए कैसे बनाया जाता है।

```Rust
use serde_yaml::Value; // यूज करें "serde_yaml" फैक्टरी क्लास "Value" को पाने के लिए

// प्रतिदिन विश्वसनीय संदर्भ
let data = r#"---
name: John Doe
age: 25
job: Software Engineer
favorites:
  - food: Pizza
  - movie: Inception
"#;

// डेटा को यैमल द्वारा पार्स करना
let data_parsed: Value = serde_yaml::from_str(data).unwrap();

// डेटा के लिए कुंजी-मूल्य पेयर संदर्भ करना
let name = data_parsed["name"].as_str().unwrap();

// प्रिंट करना
println!("नाम: {}", name);

/* आउटपुट:
नाम: John Doe
*/ 
```

## गहराई में

यैमल एक अत्यंत उत्तम डेटा संरचना है जो Rust में बहुत सरल है और इसके लाभों को चिह्नित करता है। यह विशेषताएं शामिल हैं:

- स्ट्रिंग, संख्या, बूलियन और अन्य मूल डेटा प्रकारों को समर्पित करना।
- गहरी और नेस्टेड डेटा संरचनाओं का समर्थन करना।
- लंबा, कारगुज़ार रेकॉर्डों को पारित करना।
- पाठ