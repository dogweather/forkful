---
title:                "कोड को फ़ंक्शन्स में व्यवस्थित करना"
date:                  2024-01-28T23:02:50.956450-07:00
model:                 gpt-4-0125-preview
simple_title:         "कोड को फ़ंक्शन्स में व्यवस्थित करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, dogweather, reviewed and added links
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
कोड को फंक्शन्स में व्यवस्थित करना विशिष्ट कार्यों को करने के लिए स्क्रिप्ट के बिट्स को एकत्रित करने के बारे में है। हम इसलिए करते हैं क्योंकि इससे कोड को पढ़ना, परीक्षण करना, और पुन: उपयोग करना आसान हो जाता है — कोई भी व्यक्ति कोड की स्पेगेटी में चलना नहीं चाहता।

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
