---
title:                "सबस्ट्रिंग निकालना"
html_title:           "Gleam: सबस्ट्रिंग निकालना"
simple_title:         "सबस्ट्रिंग निकालना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

अक्सर हमारे प्रोग्राम में इस्तेमाल होने वाले स्ट्रिंग की अलग-अलग भागों को निकालना होता है। इस प्रकार की स्ट्रिंग के भागों को हम substrings कहते हैं। यहां हम स्ट्रिंग से substrings निकालने का तरीका जानेंगे।

## कैसे करें: 
```Gleam
let string = "Hello World"
let substring = String.slice(string, 0, 5)
```
उपरोक्त उदाहरण में, हमने पहले स्ट्रिंग ```Hello World``` को string नामक एक वेरिएबल में स्टोर किया है। फिर हमने ```String.slice``` मेथड का उपयोग करके इस स्ट्रिंग का सबसे पहला भाग (0 से 5 तक) निकाला है और उसे substring नामक वेरिएबल में स्टोर किया है। अब हम substring को प्रिंट कर सकते हैं और उसका आउटपुट होगा ```Hello```।

## गहराई से जानें: 
इस प्रकार से स्ट्रिंग से substrings निकालना आसान और उपयोगी है लेकिन क्या आप जानते हैं कि substrings निकालने का यह तरीका Elixir भाषा से लिया गया है? अन्य तरीकों में स्ट्रिंग से substrings निकालने के लिए Elixir के अलावा Python का उपयोग किया जाता है। substrings को निकालने का यह तरीका अक्सर स्ट्रिंग मैनिपुलेशन को सरल बनाता है। 

## इससे जुड़े और भी कुछ: 
यदि आप स्ट्रिंग मैनिपुलेशन के और तरीकों को सीखना चाहते हैं, तो आप निम्नलिखित स्रोतों को देख सकते हैं। 

- [Gleam डॉक्यूमेंटेशन](https://gleam.run/documentation)
- [Elixir डॉक्यूमेंटेशन](https://elixir-lang.org/getting-started/introduction.html)
- [Python डॉक्यूमेंटेशन](https://www.python.org/about/gettingstarted/)