---
title:    "Elm: नियमित अभिव्यंजनों का उपयोग"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## क्यों

यदि आप अपनी वेब डेवलपमेंट कैरियर को आगे बढ़ाना चाहते हैं, तो आपको रेगुलर एक्सप्रेशन का उपयोग करना सीखना चाहिए। यह शक्तिशाली टूल आपको अपने प्रोग्राम में स्ट्रिंग पैटर्न को पहचानने और विभिन्न स्ट्रिंग मोडिफायर करने की अनुमति देता है।

## कैसे करें

आइए देखें कि रेगुलर एक्सप्रेशन का उपयोग कैसे करें। ```Elm String``` मॉड्यूल में हमें स्ट्रिंगों को संशोधित करने के लिए विभिन्न फंक्शन्स प्रदान किए गए हैं। हम यहां स्ट्रिंग का पाठांकन ("matching") और उसे एक अन्य स्ट्रिंग से बदलने के लिए रेगुलर एक्सप्रेशन का उपयोग करेंगे।

```elm
import String

-- स्ट्रिंग का पाठांकन करने के लिए रेगुलर एक्सप्रेशन का उपयोग
regex := Regex.fromRegex "/[a-z]+/" -- एक स्ट्रिंग में दो सीक्वेंस के बीच के शब्दों का पाठांकन करने के लिए
String.regex regex "hello world" -- ["hello", "world"]

-- एक स्ट्रिंग में से दो सीक्वेंस को हटाने के लिए
String.replace (Regex.fromRegex "world") "elm" "hello world" -- "hello elm"
```

## गहराई में जाएं

रेगुलर एक्सप्रेशन में भावनात्मक शब्द और संख्याओं को पहचानना आसान होता है। आप संख्याओं और शब्दों के गुणवत्ता को साधारण एक्सप्रेशन के साथ भी जोड़ सकते हैं। हालांकि, यदि आप समझ गए हैं कि आपको यह डाटा तलाशने के लिए एक और उपकरण चाहिए है, तो आप कमिज़्ज़बा रोक सकते हैं। Elm निजता को बेह