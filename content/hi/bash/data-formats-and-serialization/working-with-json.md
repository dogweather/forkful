---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:21.291700-07:00
description: "Bash \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\
  \u0917 \u092E\u0947\u0902 JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E\
  \ \u0915\u0930\u0928\u093E \u0906\u0926\u0947\u0936 \u092A\u0902\u0915\u094D\u0924\
  \u093F \u0938\u0947 \u0938\u0940\u0927\u0947 JSON \u0921\u0947\u091F\u093E \u0915\
  \u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947, \u0928\u093F\u0915\
  \u093E\u0932\u0928\u0947 \u0914\u0930 \u0938\u0902\u0936\u094B\u0927\u093F\u0924\
  \ \u0915\u0930\u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\
  \u092F\u093E \u0936\u093E\u092E\u093F\u0932 \u0939\u0948\u0964 \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0905\u0915\u094D\u0938\u0930\u2026"
lastmod: '2024-03-13T22:44:52.663827-06:00'
model: gpt-4-0125-preview
summary: "Bash \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917\
  \ \u092E\u0947\u0902 JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\
  \u0930\u0928\u093E \u0906\u0926\u0947\u0936 \u092A\u0902\u0915\u094D\u0924\u093F\
  \ \u0938\u0947 \u0938\u0940\u0927\u0947 JSON \u0921\u0947\u091F\u093E \u0915\u094B\
  \ \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947, \u0928\u093F\u0915\u093E\
  \u0932\u0928\u0947 \u0914\u0930 \u0938\u0902\u0936\u094B\u0927\u093F\u0924 \u0915\
  \u0930\u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\
  \u093E \u0936\u093E\u092E\u093F\u0932 \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930 \u0905\u0915\u094D\u0938\u0930\u2026"
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
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
