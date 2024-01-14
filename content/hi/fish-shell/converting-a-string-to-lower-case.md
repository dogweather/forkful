---
title:                "Fish Shell: स्ट्रिंग को लोअर केस में रूपांतरित करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरित करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों
निचे लाइन को कम केस में रूपांतरित करना हमारे कम केस स्ट्रिंग को आसानी से पठनीय बनाता है और कोडिंग को सरल बनाता है।

## कैसे करें
```Fish Shell
set str Hello World
echo $str | tr A-Z a-z
```
इस कोड के द्वारा, हमने "Hello World" स्ट्रिंग को "hello world" में बदल दिया है। यहाँ `tr` कमांड का उपयोग हो रहा है, जो की एक बहुत ही सरल और शक्तिशाली उपकरण है। यह स्ट्रिंग को कुछ ही सेकंड में लोअर केस में रूपांतरित कर देता है।

## गहराई में जाने
कम केस में स्ट्रिंग करना एक बहुत ही उपयोगी क्रिया है। इससे न केवल हमारे कोड को सरल बनाया जा सकता है, बल्कि उसे समझना भी आसान हो जाता है। हम किसी भी भाषा में लिखे गए कोड को लोअर केस में रूपांतरित कर सकते हैं, जो की भाषा को संभाव्यता और भाषा का भाव मजबूत करता है। 

## देखें भी
इस ब्लॉग पोस्ट के साथ कुछ और उपयोगी संसाधनों को देखें। 

- [Fish Shell आधिकारिक वेबसाइट](https://fishshell.com)
- [Fish Shell कमांड लाइन उपकरण](https://github.com/fish-shell/fish-shell)
- [Fish Shell संपूर्ण डॉक्यूमेंटेशन](https://fishshell.com/docs/current/index.html)