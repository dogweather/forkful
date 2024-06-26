---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:42.755548-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Bash \u092E\u0947\
  \u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B\
  \ \u0915\u0948\u092A\u093F\u091F\u0932\u093E\u0907\u091C\u093C \u0915\u0930\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F \u0935\u093F\u0936\u0947\u0937 \u0930\u0942\
  \u092A \u0938\u0947 \u092C\u0928\u093E\u092F\u093E \u0917\u092F\u093E \u090F\u0915\
  \ \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u092B\u0902\u0915\u094D\u0936\u0928\
  \ \u0928\u0939\u0940\u0902 \u0939\u0948, \u092A\u0930\u0902\u0924\u0941 \u0906\u092A\
  \ \u092F\u0939 \u0915\u093E\u0930\u094D\u092F \u092A\u0948\u0930\u093E\u092E\u0940\
  \u091F\u0930 \u090F\u0915\u094D\u0938\u092A\u0948\u0902\u0936\u0928 \u092F\u093E\
  \ \u092C\u093E\u0939\u0930\u0940\u2026"
lastmod: '2024-03-13T22:44:52.593333-06:00'
model: gpt-4-0125-preview
summary: "Bash \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \u094D\u0938 \u0915\u094B \u0915\u0948\u092A\u093F\u091F\u0932\u093E\u0907\u091C\
  \u093C \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0935\u093F\u0936\
  \u0947\u0937 \u0930\u0942\u092A \u0938\u0947 \u092C\u0928\u093E\u092F\u093E \u0917\
  \u092F\u093E \u090F\u0915 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u092B\u0902\
  \u0915\u094D\u0936\u0928 \u0928\u0939\u0940\u0902 \u0939\u0948, \u092A\u0930\u0902\
  \u0924\u0941 \u0906\u092A \u092F\u0939 \u0915\u093E\u0930\u094D\u092F \u092A\u0948\
  \u0930\u093E\u092E\u0940\u091F\u0930 \u090F\u0915\u094D\u0938\u092A\u0948\u0902\u0936\
  \u0928 \u092F\u093E \u092C\u093E\u0939\u0930\u0940 \u091F\u0942\u0932\u094D\u0938\
  \ \u091C\u0948\u0938\u0947 `awk` \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0915\u0947 \u0938\u0902\u092A\u093E\u0926\u093F\u0924 \u0915\u0930 \u0938\
  \u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\u0901 Bash \u092E\
  \u0947\u0902 \u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\
  \u094B \u0915\u0948\u092A\u093F\u091F\u0932\u093E\u0907\u091C\u093C \u0915\u0930\
  \u0928\u0947 \u0915\u0947 \u0915\u0941\u091B \u0924\u0930\u0940\u0915\u0947 \u0926\
  \u093F\u090F \u0917\u090F \u0939\u0948\u0902."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\u0948\
  \u092A\u093F\u091F\u0932\u093E\u0907\u091C \u0915\u0930\u0928\u093E"
weight: 2
---

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
