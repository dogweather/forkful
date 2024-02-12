---
title:                "स्ट्रिंग को कैपिटलाइज करना"
aliases:
- hi/fish-shell/capitalizing-a-string.md
date:                  2024-02-03T19:06:07.418849-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

एक स्ट्रिंग का पहला अक्षर बड़ा (कैपिटल) करना और शेष स्ट्रिंग को छोटा (लोअरकेस) करना इसका अर्थ होता है। यह एक आम कार्य है जिसे टेक्स्ट प्रोसेसिंग, यूजर इनपुट नॉर्मलाइज़ेशन, और डेटा फॉर्मेटिंग में सुसंगतता सुनिश्चित करने या विशिष्ट फॉर्मेटिंग मानदंडों को पूरा करने के लिए किया जाता है।

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
