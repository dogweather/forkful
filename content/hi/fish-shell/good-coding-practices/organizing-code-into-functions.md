---
changelog:
- 2024-01-28, dogweather, reviewed and added links
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:02:50.956450-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u092B\u093F\u0936\
  \ \u092E\u0947\u0902, \u0906\u092A `function` \u0915\u0940\u0935\u0930\u094D\u0921\
  \ \u0915\u0947 \u0938\u093E\u0925 \u090F\u0915 \u092B\u0902\u0915\u094D\u0936\u0928\
  \ \u0932\u093F\u0916\u0924\u0947 \u0939\u0948\u0902, \u0909\u0938\u0947 \u090F\u0915\
  \ \u0928\u093E\u092E \u0926\u0947\u0924\u0947 \u0939\u0948\u0902, \u0914\u0930 `end`\
  \ \u0915\u0947 \u0938\u093E\u0925 \u0938\u092E\u093E\u092A\u094D\u0924 \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u0938\
  \u0930\u0932 \u0935\u093E\u0932\u093E \u0939\u0948."
lastmod: '2024-03-13T22:44:53.075907-06:00'
model: gpt-4-0125-preview
summary: "\u092B\u093F\u0936 \u092E\u0947\u0902, \u0906\u092A `function` \u0915\u0940\
  \u0935\u0930\u094D\u0921 \u0915\u0947 \u0938\u093E\u0925 \u090F\u0915 \u092B\u0902\
  \u0915\u094D\u0936\u0928 \u0932\u093F\u0916\u0924\u0947 \u0939\u0948\u0902, \u0909\
  \u0938\u0947 \u090F\u0915 \u0928\u093E\u092E \u0926\u0947\u0924\u0947 \u0939\u0948\
  \u0902, \u0914\u0930 `end` \u0915\u0947 \u0938\u093E\u0925 \u0938\u092E\u093E\u092A\
  \u094D\u0924 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\
  \u0901 \u090F\u0915 \u0938\u0930\u0932 \u0935\u093E\u0932\u093E \u0939\u0948."
title: "\u0915\u094B\u0921 \u0915\u094B \u092B\u093C\u0902\u0915\u094D\u0936\u0928\
  \u094D\u0938 \u092E\u0947\u0902 \u0935\u094D\u092F\u0935\u0938\u094D\u0925\u093F\
  \u0924 \u0915\u0930\u0928\u093E"
weight: 18
---

## कैसे करें:
फिश में, आप `function` कीवर्ड के साथ एक फंक्शन लिखते हैं, उसे एक नाम देते हैं, और `end` के साथ समाप्त करते हैं। यहाँ एक सरल वाला है:

```fish
function hello
    echo "Hello, World!"
end

hello
```

आउटपुट:
```
Hello, World!
```

अब, चलिए इसे उपयोगकर्ता को अभिवादन करने दें:

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

आउटपुट:
```
Hey there, आपका_उपयोगकर्ता_नाम!
```

इसे सत्रों के बीच सहेजने के लिए, `funcsave greet` का उपयोग करें।

## गहराई में जानकारी
फिश शेल फंक्शन्स मिनी-स्क्रिप्ट्स की तरह होते हैं — आप वहाँ लगभग कुछ भी डाल सकते हैं। ऐतिहासिक रूप से, शेल स्क्रिप्टिंग में फंक्शन्स की अवधारणा ने दोहराव वाले टाइपिंग और डिबगिंग के अनगिनत घंटों को बचाया है। पाइथन जैसी प्रोग्रामिंग भाषाओं के विपरीत, शेल फंक्शन्स अधिक संरचना की तुलना में सुविधा के बारे में होते हैं।

कुछ शेल्स, जैसे कि बैश, `function` या सीधे ब्रेसेस का उपयोग करते हैं। फिश `function ... end` पर टिकी रहती है— स्पष्ट और पठनीय। फिश फंक्शन्स के अंदर, आपको सभी संकेत मिलते हैं: पैरामीटर, `set -l` के साथ स्थानीय चर, और आप एक फंक्शन के अंदर एक और फंक्शन भी परिभाषित कर सकते हैं।

आपको `return` मूल्य की आवश्यकता नहीं होगी क्योंकि फिश इस पर बड़ा नहीं है; आपके फंक्शन का आउटपुट ही इसका रिटर्न है। और यदि आप भविष्य के सत्रों में उपलब्ध स्थायी फंक्शन्स चाहते हैं, तो `funcsave` को याद रखें।

## देखें भी
- फंक्शन्स पर फिश ट्यूटोरियल: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### फंक्शन कमांड्स
- [function](https://fishshell.com/docs/current/cmds/function.html) — एक फंक्शन बनाएँ
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — फंक्शन्स को प्रिंट करें या मिटाएँ
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — उपयोगकर्ता की ऑटोलोड डायरेक्टरी में एक फंक्शन की परिभाषा को सहेजें
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — एक फंक्शन को इंटरैक्टिव रूप से संपादित करें
