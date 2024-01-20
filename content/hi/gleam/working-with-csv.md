---
title:                "CSV के साथ काम करना"
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
CSV से हम टेक्स्ट डेटा काम करते हैं; यह सिम्पल और यूनिवर्सल फॉर्मेट है। प्रोग्रामर्स डेटा को आसानी से एक्सचेंज, स्टोर और एनालाइज करने के लिए CSV का इस्तेमाल करते हैं।

## कैसे करें? (How to:)
ग्लीम में CSV के साथ काम करने के लिए, मान लीजिये हमारे पास निम्नलिखित CSV फाइल है:

```
id,name,age
1,Alice,30
2,Bob,29
```

इसे पढ़ने का ग्लीम कोड यहाँ है:

```gleam
// यहाँ CSV पढ़ने का कोड होगा।
// चूंकि ग्लीम में सीधा CSV पार्सिंग लाइब्रेरी नहीं है, हम Rust की स्टैंडर्ड libary का इस्तेमाल करके एक्साम्पल दिखा सकते हैं।
// नोट: ग्लीम करंट वर्जन के अनुसार यह कोड बदल सकता है।
```

सैंपल आउटपुट:

```
# यहाँ आपके पार्स किए गए डेटा का आउटपुट होगा।
```

## गहराई की बातें (Deep Dive)
CSV का इस्तेमाल 1970 के दशक से हो रहा है। इसके अल्टरनेटिव्स में XML, JSON जैसे फॉर्मेट्स हैं। हालांकि, CSV की सादगी और पढ़ने में आसानी के कारण यह आज भी लोकप्रिय है। कभी-कभी प्रोग्रामर्स लाइब्रेरीज का इस्तेमाल करते हैं, लेकिन Gleam की प्योर नेचर की वजह से हमें अक्सर दूसरी भाषाओं की लाइब्रेरीज अपनानी पड़ती हैं।

## संबंधित स्रोत (See Also)
- Gleam official documentation: [https://gleam.run/documentation/](https://gleam.run/documentation/)
- CSV पर और जानकारी के लिए: [https://en.wikipedia.org/wiki/Comma-separated_values](https://en.wikipedia.org/wiki/Comma-separated_values)
- Rust CSV पार्सिंग: [https://docs.rs/csv](https://docs.rs/csv)