---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:07.418849-07:00
description: "\u0915\u0948\u0938\u0947: Fish Shell \u092E\u0947\u0902, \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B \u0938\u0940\u0927\
  \u0947 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u092B\u0902\u0915\u094D\u0936\
  \u0902\u0938 \u0915\u0947 \u0938\u093E\u0925 \u092E\u0948\u0928\u093F\u092A\u094D\
  \u092F\u0941\u0932\u0947\u091F \u0915\u093F\u092F\u093E \u091C\u093E \u0938\u0915\
  \u0924\u093E \u0939\u0948, \u092C\u093E\u0939\u0930\u0940 \u091F\u0942\u0932\u094D\
  \u0938 \u092F\u093E \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940\u091C\
  \u093C \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E \u0915\u0947\
  \ \u092C\u093F\u0928\u093E\u0964 \u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\
  \u0902\u0917\u2026"
lastmod: '2024-03-13T22:44:53.033484-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell \u092E\u0947\u0902, \u0938\u094D\u091F\u094D\u0930\u093F\u0902\
  \u0917\u094D\u0938 \u0915\u094B \u0938\u0940\u0927\u0947 \u092C\u093F\u0932\u094D\
  \u091F-\u0907\u0928 \u092B\u0902\u0915\u094D\u0936\u0902\u0938 \u0915\u0947 \u0938\
  \u093E\u0925 \u092E\u0948\u0928\u093F\u092A\u094D\u092F\u0941\u0932\u0947\u091F\
  \ \u0915\u093F\u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948, \u092C\
  \u093E\u0939\u0930\u0940 \u091F\u0942\u0932\u094D\u0938 \u092F\u093E \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u0940\u091C\u093C \u0915\u0940 \u0906\u0935\
  \u0936\u094D\u092F\u0915\u0924\u093E \u0915\u0947 \u092C\u093F\u0928\u093E\u0964\
  \ \u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\
  \u0948\u092A\u093F\u091F\u0932\u093E\u0907\u091C\u093C \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F, \u0906\u092A `string` \u0915\u092E\u093E\u0902\
  \u0921 \u0915\u094B \u0938\u092C\u0915\u092E\u093E\u0902\u0921\u094D\u0938 \u0915\
  \u0947 \u0938\u093E\u0925 \u091C\u094B\u0921\u093C \u0938\u0915\u0924\u0947 \u0939\
  \u0948\u0902\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\u0948\
  \u092A\u093F\u091F\u0932\u093E\u0907\u091C \u0915\u0930\u0928\u093E"
weight: 2
---

## कैसे:
Fish Shell में, स्ट्रिंग्स को सीधे बिल्ट-इन फंक्शंस के साथ मैनिप्युलेट किया जा सकता है, बाहरी टूल्स या लाइब्रेरीज़ की आवश्यकता के बिना। एक स्ट्रिंग को कैपिटलाइज़ करने के लिए, आप `string` कमांड को सबकमांड्स के साथ जोड़ सकते हैं।

```fish
# नमूना स्ट्रिंग
set sample_string "hello world"

# पहले अक्षर को कैपिटलाइज़ करें
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

आउटपुट:
```
Hello world
```

ऐसी स्थितियों के लिए जहां एक स्ट्रिंग में कई शब्दों को कैपिटलाइज़ करना हो ("hello world" को "Hello World" में परिवर्तित करना), आप प्रत्येक शब्द पर लूप करेंगे, प्रत्येक पर कैपिटलाइज़ेशन लॉजिक लागू करते हुए:

```fish
# नमूना वाक्य
set sentence "hello fish shell programming"

# प्रत्येक शब्द को कैपिटलाइज़ करें
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# कैपिटलाइज़्ड शब्दों को जोड़ें
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

आउटपुट:
```
Hello Fish Shell Programming
```

ध्यान दें कि Fish Shell सीधे तौर पर कुछ प्रोग्रामिंग भाषाओं के स्ट्रिंग मेथड्स के समान पूरे वाक्य कैपिटलाइज़ेशन के लिए एकल-कमांड दृष्टिकोण की पेशकश नहीं करता। इसलिए, `string split`, `string sub`, `string upper` को जोड़ने और फिर पुन: जोड़ने का तरीका Fish Shell में इसे प्राप्त करने के लिए एक इडियोमेटिक दृष्टिकोण प्रस्तुत करता है।
