---
title:                "JSON के साथ काम करना"
date:                  2024-02-03T19:22:21.291700-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Bash प्रोग्रामिंग में JSON के साथ काम करना आदेश पंक्ति से सीधे JSON डेटा को पार्स करने, निकालने और संशोधित करने की प्रक्रिया शामिल है। प्रोग्रामर अक्सर इसे वेब APIs और आधुनिक डेटा आदान-प्रदान प्रारूपों के साथ शेल स्क्रिप्ट्स को सहजता से एकीकृत करने के लिए करते हैं, जिससे Bash स्क्रिप्टिंग को एक JSON-भारी पारिस्थितिकी तंत्र में अधिक शक्तिशाली और प्रासंगिक बनाया जा सकता है।

## कैसे करें:
Bash में अपने आप में JSON पार्सिंग क्षमताएं नहीं होती हैं, लेकिन `jq` एक शक्तिशाली कमांड-लाइन JSON प्रोसेसर है जो इस अंतर को भरता है। इसका उपयोग कैसे करें, यहाँ है:

**एक JSON फ़ाइल पढ़ना:**

नमूना `data.json`:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

JSON फ़ाइल से नाम निकालने के लिए:
```bash
jq '.name' data.json
```
आउटपुट:
```
"Jane Doe"
```

**JSON डेटा संशोधित करना:**

शहर को "Los Angeles" में अपडेट करने और फाइल में वापस लिखने के लिए:
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**एक वेरिएबल से JSON पार्स करना:**

यदि आपके पास एक Bash वेरिएबल में JSON है, तो `jq` इसे भी प्रोसेस कर सकता है:
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
आउटपुट:
```
"John Doe"
```

**सरणियों के साथ काम करना:**

JSON में आइटम्स की एक सरणी दी गई है:
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

दूसरा आइटम निकालने के लिए (अनुक्रमण 0 से शुरू होता है):
```bash
jq '.items[1]' data.json
```
आउटपुट:
```
"banana"
```

अधिक जटिल संचालनों और फ़िल्टरिंग के लिए, `jq` के पास ऑनलाइन उपलब्ध एक व्यापक मैनुअल और ट्यूटोरियल हैं, जिसे आपकी सभी बैश/JSON आवश्यकताओं के लिए एक बहुमुखी उपकरण बनाते हैं।
