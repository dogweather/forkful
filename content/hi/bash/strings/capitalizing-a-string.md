---
title:                "स्ट्रिंग को कैपिटलाइज करना"
date:                  2024-02-03T19:05:42.755548-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Bash में एक स्ट्रिंग को कैपिटलाइज़ करना इसके पहले अक्षर को अपरकेस में परिवर्तित करने और बाकी स्ट्रिंग को अपरिवर्तित छोड़ने की प्रक्रिया है। यह तकनीक सामान्यतः आउटपुट को फॉर्मेट करने या कोडिंग कन्वेंशन्स के साथ अनुपालन करने के लिए उपयोग की जाती है जिसमें कुछ स्ट्रिंग्स को पठनीयता या शैलीगत प्राथमिकताओं के लिए एक कैपिटल अक्षर से शुरू होना आवश्यक होता है।

## कैसे करें:

Bash में स्ट्रिंग्स को कैपिटलाइज़ करने के लिए विशेष रूप से बनाया गया एक बिल्ट-इन फंक्शन नहीं है, परंतु आप यह कार्य पैरामीटर एक्सपैंशन या बाहरी टूल्स जैसे `awk` का उपयोग करके संपादित कर सकते हैं। यहाँ Bash में एक स्ट्रिंग को कैपिटलाइज़ करने के कुछ तरीके दिए गए हैं:

**पैरामीटर एक्सपैंशन का उपयोग करना:**

यह विधि सीधे शेल में स्ट्रिंग को मैनिपुलेट करती है।

```bash
str="hello world"
capitalized="${str^}"
echo "$capitalized"
```
आउटपुट:
```
Hello world
```

**`awk` का उपयोग करना:**

`awk` अधिकांश Unix-जैसे ऑपरेटिंग सिस्टम्स पर उपलब्ध एक शक्तिशाली पाठ प्रोसेसिंग टूल है, जिसे स्ट्रिंग्स को कैपिटलाइज़ करने के लिए उपयोग में लाया जा सकता है।

```bash
str="hello world"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
आउटपुट:
```
Hello world
```

**`sed` का उपयोग करना:**

एक पारंपरिक दृष्टिकोण के लिए, `sed` का उपयोग करके स्ट्रिंग के पहले अक्षर को कैपिटलाइज़ किया जा सकता है। हालांकि, यह पिछली विधियों की तुलना में थोड़ा अधिक जटिल है।

```bash
str="hello world"
echo "$str" | sed 's/./\u&/'
```
आउटपुट:
```
Hello world
```

ये स्निपेट्स Bash में एक स्ट्रिंग के पहले अक्षर को कैपिटलाइज़ करने के तरीकों को दर्शाते हैं, जो पाठ को मैनिपुलेट करते समय शेल स्क्रिप्टिंग की लचीलापन को उजागर करते हैं।