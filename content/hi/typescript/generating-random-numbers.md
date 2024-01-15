---
title:                "कम्प्यूटर प्रोग्रामिंग पर 'रैंडम नंबर्स उत्पन्न करना'"
html_title:           "TypeScript: कम्प्यूटर प्रोग्रामिंग पर 'रैंडम नंबर्स उत्पन्न करना'"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर 'रैंडम नंबर्स उत्पन्न करना'"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें एक randomly generated number चाहिए होता है, जैसे एक गेम में स्कोरिंग के लिए या अपने वेबसाइट के लिए उपयुक्त पासवर्ड बनाने के लिए। इसलिए, TypeScript में random numbers generate करना अत्यंत महत्वपूर्ण है।

## कैसे करें

यदि आपने पहले से ही TypeScript सीखा है, तो आपको random numbers generate करना आसान होगा। नीचे दिए गए कुछ उदाहरण देखें:

```TypeScript
// एक से दस तक के बीच में random numbers generate करना
let randomNumber1: number = Math.floor(Math.random() * 10);

// शून्य से एक से सौ तक के बीच में random numbers generate करना
let randomNumber2: number = Math.floor(Math.random() * 100) + 1;

// दो दशमलव अंकों तक के random numbers generate करना
let randomNumber3: number = Math.random() * 10;

// सेहिज समय से उपलब्ध true या false के बीच में random boolean generate करना
let randomBool: boolean = Math.random() < 0.5;
```

उपरोक्त कोड में, `Math.random()` हमें तीसरे दशमलव अंक तक एक औसत नंबर प्रदान करता है, जो हम फिर से `Math.floor()` के साथ इसे पूरा नंबर बनाने के लिए उपयोग करते हैं।

## गहराई जानकारी

जब हम `Math.random()` का उपयोग करते हैं, तो हम वास्तव में टीएस के बाहर कोई external लाइब्रेरी कॉल कर रहे हैं। यह लाइब्रेरी संभवतः प्रत्येक JavaScript runtime environment में मौजूद होती है और हमें एक अतिरिक्त विश्लेषण नंबर प्रदान करती है।

इसके अलावा, हम भी अपने इच्छानुसार समय के आधार पर random numbers generate कर सकते हैं। उदाहरण के लिए, हम `Date.now()` का उपयोग करके वर्तमान समय से अलग अलग random numbers generate कर सकते हैं।

## और भी देखें

- [MDN के आरंभिक तरीके से random