---
title:                "यामल के साथ काम करना"
date:                  2024-01-19
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

YAML एक डेटा सीरियलाइजेशन फॉर्मेट है, जो डेटा को इंसान के पढ़ने योग्य फॉर्म में दिखाता है। प्रोग्रामर इसका इस्तेमाल कॉन्फ़िगुरेशन फाइलों, डेटा स्टोरेज, और मैसेज फॉर्मेटिंग के लिए करते हैं क्योंकि यह आसानी से समझा और लिखा जा सकता है।

## कैसे करें:

Gleam में YAML का इस्तेमाल करने के लिए अभी तक ऑफिशियल लाइब्रेरी नहीं है। आप Rust की `serde_yaml` का इस्तेमाल करके FFI के ज़रिए YAML पढ़ और लिख सकते हैं। यहाँ एक सामान्य उदाहरण है:

```gleam
external fn parse_yaml(String) -> Result(Map(String, String), Nil) =
  "yaml_parser" "parse"
```

परिणाम को एक मैप में बदल दिया जाता है, जहां प्रत्येक कुंजी और मान स्ट्रिंग के रूप में होते हैं।

## गहराई में:

YAML यानी "YAML Ain't Markup Language" (पुराना अर्थ: "Yet Another Markup Language") 2001 में बना था। इसे अक्सर JSON और XML की विकल्प के रूप में देखा जाता है। YAML के डिज़ाइन में पठनीयता का बहुत ध्यान रखा गया है, और इसका सिंटैक्स अधिक स्वाभाविक और सहज है। Gleam में YAML के इम्प्लीमेंटेशन के लिए FFI (Foreign Function Interface) की आवश्यकता होती है, जिसके लिए आमतौर पर Rust या C जैसी भाषाएं इस्तेमाल की जाती हैं।

## यह भी देखें:

- YAML ऑफिशियल वेबसाइट: [https://yaml.org/](https://yaml.org/)
- `serde_yaml` Rust क्रेट: [https://docs.rs/serde_yaml/](https://docs.rs/serde_yaml/)
