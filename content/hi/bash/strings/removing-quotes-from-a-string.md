---
date: 2024-01-26 03:38:59.464794-07:00
description: "\u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\
  \u0947 \u0909\u0926\u094D\u0927\u0930\u0923 \u091A\u093F\u0939\u094D\u0928 \u0939\
  \u091F\u093E\u0928\u093E, \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\
  \u094B \u092C\u0902\u0926 \u0915\u0930\u0928\u0947 \u0935\u093E\u0932\u0947 \u0909\
  \u0926\u094D\u0927\u0930\u0923 \u091A\u093F\u0939\u094D\u0928\u094B\u0902 \u0915\
  \u094B \u0939\u091F\u093E\u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\
  \u0930\u093F\u092F\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930 \u0905\u0915\u094D\u0938\u0930 \u0907\u0928\u092A\u0941\
  \u091F \u0921\u0947\u091F\u093E \u0915\u094B \u0938\u0948\u0928\u093F\u091F\u093E\
  \u0907\u091C\u093C \u0915\u0930\u0928\u0947, \u0921\u0947\u091F\u093E\u2026"
lastmod: '2024-03-13T22:44:52.602228-06:00'
model: gpt-4-0125-preview
summary: "\u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947\
  \ \u0909\u0926\u094D\u0927\u0930\u0923 \u091A\u093F\u0939\u094D\u0928 \u0939\u091F\
  \u093E\u0928\u093E, \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B\
  \ \u092C\u0902\u0926 \u0915\u0930\u0928\u0947 \u0935\u093E\u0932\u0947 \u0909\u0926\
  \u094D\u0927\u0930\u0923 \u091A\u093F\u0939\u094D\u0928\u094B\u0902 \u0915\u094B\
  \ \u0939\u091F\u093E\u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\
  \u093F\u092F\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\
  \u093E\u092E\u0930 \u0905\u0915\u094D\u0938\u0930 \u0907\u0928\u092A\u0941\u091F\
  \ \u0921\u0947\u091F\u093E \u0915\u094B \u0938\u0948\u0928\u093F\u091F\u093E\u0907\
  \u091C\u093C \u0915\u0930\u0928\u0947, \u0921\u0947\u091F\u093E\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0909\u0926\
  \u094D\u0927\u0930\u0923 \u091A\u093F\u0939\u094D\u0928 \u0939\u091F\u093E\u0928\
  \u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग से उद्धरण चिह्न हटाना, स्ट्रिंग को बंद करने वाले उद्धरण चिह्नों को हटाने की प्रक्रिया है। प्रोग्रामर अक्सर इनपुट डेटा को सैनिटाइज़ करने, डेटा की तुलना के लिए तैयार करने, या अन्य प्रोग्राम या सिस्टम के साथ इंटरफेसिंग करते समय एक विशेष डेटा प्रारूप का पालन करने के लिए ऐसा करना चाहते हैं।

## कैसे करें:
Bash में स्ट्रिंग्स से उद्धरण चिह्न हटाने के कई तरीके हैं। यहाँ कुछ सरल उदाहरण दिए गए हैं:

```Bash
#!/bin/bash

# दोनों सिंगल और डबल उद्धरण हटाने के लिए वेरिएबल प्रतिस्थापन का उपयोग करना
STRING="\"Hello, World!\""
echo ${STRING//\"}

# `tr` का उपयोग करके उद्धरण हटाना
STRING="'Hello, World!'"
echo $STRING | tr -d "\'"

# `sed` का उपयोग करके उद्धरण हटाना
STRING="\"Hello, World!\""
echo $STRING | sed 's/"//g'
```

नमूना आउटपुट:

```
Hello, World!
Hello, World!
Hello, World!
```

## गहराई में जाएँ
जब बात पाठ प्रोसेसिंग की आती है, पहले Unix कमांडों जैसे `tr` और `sed` प्रमुख उपकरण हुआ करते थे। वे आज भी उनकी लचीलापन और पाठ परिवर्तनों जैसे उद्धरण हटाने में शक्ति के लिए इस्तेमाल किए जाते हैं। वे किसी भी शैल-लिपिकार के टूलबॉक्स में एक मुख्य उपकरण हैं।

Bash स्वयं तब से विकसित हो गया है और वेरिएबल प्रतिस्थापन छोटे पैमाने पर स्ट्रिंग परिवर्तनों के लिए एक और स्तर की सादगी जोड़ता है। यह आपको बाहरी बाइनरीज़ को पाइपिंग से बचाता है, आपके स्क्रिप्ट को थोड़ा अधिक कुशल बनाता है।

जबकि `tr` वर्णों को हटाने के लिए शानदार है, यह अधिक जटिल पैटर्न को संभाल नहीं पाता। दूसरी ओर, `sed` नियमित अभिव्यक्तियों का उपयोग करता है, इसलिए कभी-कभी यह ओवरकिल हो सकता है और सरल ऑपरेशनों के लिए धीमा हो सकता है।

इन विधियों में से चयन करना आपके विशेष मामले पर निर्भर करता है। यदि आपको विभिन्न प्रकार के उद्धरण हटाने हैं और आप पहले से ही एक Bash स्क्रिप्ट के संदर्भ में हैं, तो इसकी सादगी के लिए वेरिएबल प्रतिस्थापन का उपयोग करना समझदारी है। लेकिन यदि आप पाठ स्ट्रीम्स या मल्टी-लाइन डेटा को परिवर्तित कर रहे हैं, `tr` और `sed` आपके पसंदीदा साथी हैं।

## देखें भी:
- जीएनयू Bash मैनुअल, विशेष रूप से पैरामीटर एक्सपैंशन और शेल पैरामीटर एक्सपैंशन पर अनुभाग: https://www.gnu.org/software/bash/manual/
- `tr` कमांड मैनुअल: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- `sed` स्ट्रीम संपादक अवलोकन: https://www.gnu.org/software/sed/manual/sed.html
