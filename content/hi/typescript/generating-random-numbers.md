---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:50:50.805213-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रैंडम नंबर्स उत्पन्न करना मतलब है ऐसे नंबर्स बनाना जो कि हर बार अलग-अलग हों। प्रोग्रामर्स इसका इस्तेमाल गेमिंग, सिमुलेशन और डाटा एनालिसिस में करते हैं ताकि परिणाम यादृच्छिक (random) हो।

## How to: (कैसे करें:)
```TypeScript
function getRandomNumber(min: number, max: number): number {
  // यादृच्छिक संख्या उत्पन्न करना min और max के बीच 
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// उदाहरण के लिए:
console.log(getRandomNumber(1, 10));
console.log(getRandomNumber(1, 100));
```

सैंपल आउटपुट (ये हर बार बदल सकता है):
```
4
83
```

## Deep Dive (गहराई से जानकारी):
प्राचीन काल से मनुष्यों ने रैंडमनेस का इस्तेमाल किया है - चाहे वो पासे के खेल में हो या अप्रत्याशित निर्णय लेने में। डिजिटल युग में, `Math.random()` JavaScript और TypeScript का इस्तेमाल करके रैंडम नंबर्स बनाए जाते हैं, जो कि पसुडो-रैंडम (नकली रैंडम) नंबर जेनरेटर (PRNG) पर आधारित होता है। यह असली रैंडमनेस नहीं है क्योंकि कंप्यूटर्स बहुत अनुमानित होते हैं। अगर वास्तविक रैंडमनेस चाहिए, तो क्रिप्टोग्राफिक रूप से सुरक्षित मेथोड्स की जरूरत पड़ती है जैसे कि `crypto.getRandomValues()` जो कि ब्राउज़र APIs में मौजूद है।

## See Also (और भी देखें):
- [Mozilla Developer Network on Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Mozilla Developer Network on crypto.getRandomValues()](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)
- [Stack Overflow discussions on random number generation](https://stackoverflow.com/questions/tagged/random)