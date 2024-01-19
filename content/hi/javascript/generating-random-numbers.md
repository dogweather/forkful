---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या & क्यों? (What & Why?)
यादृच्छिक संख्याओं का उत्पादन यदि आपके कोड का हिस्सा है, तो यह अद्वितीयता और अप्रत्याशितता को सुनिश्चित करता है। खेलों, एन्क्रिप्शन, एल्गोरिदमों आदि में प्रोग्रामर्स इसे उपयोग करते हैं।

## कैसे: (How to:)
Javascript में, `Math.random()` फ़ंक्शन एक यादृच्छिक संख्या उत्पन्न करता है।
```Javascript
let randomNo = Math.random();
console.log(randomNo);
```
उदाहरण के लिए, अगर आप `0` और `10` के बीच एक यादृच्छिक संख्या चाहते हैं तो,
```Javascript
let randomNo = Math.floor(Math.random() * 11);
console.log(randomNo);
```

## गहरा डाइव (Deep Dive)
यादृच्छिक संख्या उत्पन्न करने का आविष्कार 20वीं सदी की शुरुआत में हुआ था और यह क्रॉनिक श्रृंखला के नाम से जाना जाता था।

बदले में, `crypto.getRandomValues()` और Web Crypto API लागू कर सकते हैं - यादृच्छिक बाइट्स उत्पन्न करता है।

`Math.random()` उत्पन्न अविश्वसनीय यादृच्छिक संख्याएं उत्पन्न करता है। इसका परिणाम सोचम सीड (seed) का प्रयोग करता है।

## भी देखें: (See Also:)
1. [MDN Web Docs - Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
2. [MDN Web Docs - crypto.getRandomValues()](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)
3. [Wikipedia - Pseudorandomness](https://en.wikipedia.org/wiki/Pseudorandomness)