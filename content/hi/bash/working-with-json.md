---
title:                "JSON के साथ काम करना"
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JSON (JavaScript Object Notation) एक डेटा प्रारूप है जिसे संभालने के लिए Bash में काम करते हैं। यह वेब APIs और सर्वर के बीच डेटा शेयर करने के लिए मानक बन गया है, इसलिए प्रोग्रामर्स इसे समझते और इस्तेमाल करते हैं।

## How to: (कैसे करें:)
आपको JSON से डेटा संभालने के लिए `jq` टूल की जरूरत पड़ेगी।

JSON फाइल पढ़ना:
```Bash
echo '{"name": "राहुल", "age": 30}' | jq '.'
```

किसी खास कुंजी (key) का मान प्राप्त करना:
```Bash
echo '{"name": "राहुल", "age": 30}' | jq '.name'
```
आउटपुट: `"राहुल"`

एरे में नया आइटम जोड़ना:
```Bash
echo '["आम", "जामुन"]' | jq '. + ["केला"]'
```
आउटपुट: `["आम", "जामुन", "केला"]`

## Deep Dive (गहन जानकारी):
JSON, 2000 के दशक के शुरू में AJAX के साथ लोकप्रिय हुआ था। यह XML जैसे मोटे डेटा प्रारूपों का एक हल्का विकल्प है। Bash में JSON के साथ काम करने के लिए `jq` सबसे प्रचलित टूल है, पर Python की `json` लाइब्रेरी या JavaScript की नेटिव `JSON.parse()` और `JSON.stringify()` जैसे अन्य विकल्प भी हैं। बात करें `jq` की, तो इसका अपना टक्सट प्रोसेसिंग और फंक्शन के साथ एक मजबूत सिंटैक्स है।

## See Also (और देखें):
- jq आधिकारिक दस्तावेज़: [https://stedolan.github.io/jq/manual/](https://stedolan.github.io/jq/manual/)
- JSON प्रारूप के बारे में गहराई से सीखने के लिए: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
- विकिपीडिया पर JSON: [https://en.wikipedia.org/wiki/JSON](https://en.wikipedia.org/wiki/JSON)