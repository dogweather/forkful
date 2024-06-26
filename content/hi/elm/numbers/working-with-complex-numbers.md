---
date: 2024-01-26 04:40:34.872241-07:00
description: "\u0915\u0948\u0938\u0947: Elm \u092E\u0947\u0902 \u091C\u091F\u093F\u0932\
  \ \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u093E \u0938\u092E\u0930\
  \u094D\u0925\u0928 \u0928\u093F\u0930\u094D\u092E\u093F\u0924 \u0930\u0942\u092A\
  \ \u0938\u0947 \u0909\u092A\u0932\u092C\u094D\u0927 \u0928\u0939\u0940\u0902 \u0939\
  \u0948, \u0907\u0938\u0932\u093F\u090F \u0906\u092A\u0915\u094B \u0905\u092A\u0928\
  \u093E \u0938\u094D\u0935\u092F\u0902 \u0915\u093E \u092A\u094D\u0930\u0915\u093E\
  \u0930 \u0914\u0930 \u092B\u0902\u0915\u094D\u0936\u0928 \u092C\u0928\u093E\u0928\
  \u0947 \u0939\u094B\u0902\u0917\u0947\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915\
  \ \u0924\u094D\u0935\u0930\u093F\u0924 \u0938\u0947\u091F\u0905\u092A \u0939\u0948\
  ."
lastmod: '2024-03-13T22:44:52.175930-06:00'
model: gpt-4-0125-preview
summary: "Elm \u092E\u0947\u0902 \u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\
  \u092F\u093E\u0913\u0902 \u0915\u093E \u0938\u092E\u0930\u094D\u0925\u0928 \u0928\
  \u093F\u0930\u094D\u092E\u093F\u0924 \u0930\u0942\u092A \u0938\u0947 \u0909\u092A\
  \u0932\u092C\u094D\u0927 \u0928\u0939\u0940\u0902 \u0939\u0948, \u0907\u0938\u0932\
  \u093F\u090F \u0906\u092A\u0915\u094B \u0905\u092A\u0928\u093E \u0938\u094D\u0935\
  \u092F\u0902 \u0915\u093E \u092A\u094D\u0930\u0915\u093E\u0930 \u0914\u0930 \u092B\
  \u0902\u0915\u094D\u0936\u0928 \u092C\u0928\u093E\u0928\u0947 \u0939\u094B\u0902\
  \u0917\u0947\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u0924\u094D\u0935\u0930\
  \u093F\u0924 \u0938\u0947\u091F\u0905\u092A \u0939\u0948."
title: "\u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 14
---

## कैसे:
Elm में जटिल संख्याओं का समर्थन निर्मित रूप से उपलब्ध नहीं है, इसलिए आपको अपना स्वयं का प्रकार और फंक्शन बनाने होंगे। यहाँ एक त्वरित सेटअप है:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- उदाहरण का उपयोग:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum है { real = 4.0, imaginary = -2.0 }
```

## गहराई में
ऐतिहासिक रूप से, जटिल संख्याएँ हमेशा स्वीकार्य नहीं थीं। वे 16वीं शताब्दी में घातीय समीकरणों को हल करने में एक खेल बदलने वाले बन गए। अन्य भाषाओं में विकल्प, जैसे Python, जटिल संख्या समर्थन को बॉक्स के बाहर से सही परिचालन के साथ प्रदान करते हैं।  Elm में जैसा कि आपने देखा है, एक DIY दृष्टिकोण की आवश्यकता होती है। लेकिन आप इसे जरूरत के अनुसार जितना सोफिस्टिकेटेड बना सकते हैं, गुणा, भाग, और अन्य ऑपरेशन बनाकर, प्रदर्शन समस्याओं को ध्यान में रखते हुए।

## देखें भी
- Elm का आधिकारिक दस्तावेज: https://package.elm-lang.org/ अनुकूलित प्रकार बनाने और Elm की मूल बातों को महारत हासिल करने के लिए।
- गणित इतिहास के शौकीन "एन इमैजिनरी टेल" द्वारा Paul J. Nahin को देख सकते हैं जो जटिल संख्याओं की समय के साथ यात्रा की कहानी कहता है।
- गणित-ओरिएंटेड प्रोग्रामिंग चुनौतियों पर Project Euler (https://projecteuler.net) में गोता लगाएँ ताकि आप अपने जटिल संख्या के जादू को लागू कर सकें।
