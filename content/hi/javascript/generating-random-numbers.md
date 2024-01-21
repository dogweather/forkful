---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:49:58.973520-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
जावास्क्रिप्ट में यादृच्छिक संख्याएँ (random numbers) उत्पन्न करना मतलब है कि प्रत्येक बार कोड चलते समय अलग-अलग संख्याओं का प्राप्त करना। गेम्स, सिमुलेशन, टेस्टिंग, और सुरक्षा (जैसे कि OTPs में) में अक्सर ऐसी संख्याओं की जरूरत होती है।

## How to: (कैसे करें:)
```Javascript
// सिंपल रैंडम नंबर 0 से 1 के बीच
console.log(Math.random());

// 1 से 100 के बीच रैंडम इंटेजर के लिए
console.log(Math.floor(Math.random() * 100) + 1);
```
सैंपल आउटपुट:
```
0.432423432423
57
```
`Math.random()` फंक्शन एक यादृच्छिक संख्या देता है, जिसे हम आवश्यकता अनुसार संशोधित कर सकते हैं।

## Deep Dive (गहराई में जानकारी)
जावास्क्रिप्ट के शुरुआती दिनों से `Math.random()` यादृच्छिक संख्याएँ उत्पन्न करने का मानक तरीका रहा है। यह एक पीआरएनजी (Pseudorandom Number Generator) है, जो वास्तविक यादृच्छिकता नहीं होती, लेकिन प्रैक्टिकल यूज़ के लिए पर्याप्त होती है। यदि आपको क्रिप्टोग्राफिक रूप से सुरक्षित यादृच्छिक संख्याओं की आवश्यकता हो, `crypto.getRandomValues()` विकल्प देखें।

## See Also (और भी देखें)
- MDN Web Docs के [Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random) पेज पर जाकर आप और भी गहराई से समझ सकते हैं।