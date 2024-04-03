---
date: 2024-01-20 17:38:48.521086-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.967994-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
weight: 4
---

## How to: (कैसे करें:)
```Javascript
// स्ट्रिंग को lower case में बदलने के लिए JavaScript में toLowerCase() मेथड का प्रयोग करते हैं।

let originalString = "नमस्ते JavaScript!";
let lowerCaseString = originalString.toLowerCase();

console.log(lowerCaseString); // Output: "नमस्ते javascript!"
```

## Deep Dive (गहराई में जानकारी)
समय के साथ JavaScript में strings को handle करने के तरीके बदलते रहे हैं। `toLowerCase()` मेथड लंबे समय से मौजूद है और यह बहुत ही विश्वसनीय है। ध्यान देने वाली बात है कि `toLowerCase()` यूनिकोड (Unicode) characters का भी समर्थन करता है, जिससे विभिन्न भाषाओं के strings सही तरीके से lower case में बदले जाते हैं।

विकल्प के तौर पर, आप `toLocaleLowerCase()` मेथड भी देख सकते हैं जो स्पेसिफिक लकेल (locale) के हिसाब से lower case conversion करता है। जैसे कि तुर्की भाषा में `I` का lower case `ı` होता है, ना कि `i`।

हालांकि, कभी-कभी यह मेथड कुछ सीमाओं का सामना कर सकता है, जैसे विशेष टेक्स्ट फॉर्मेटिंग या कैसिंग शैलियों के साथ। इसलिए हमेशा अपने एप्लीकेशन की जरूरतों के हिसाब से सही मेथड का चुनाव करना चाहिए।

## See Also (और भी जानकारियाँ)
- MDN Web Docs पर `toLowerCase()` और `toLocaleLowerCase()`: [String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- w3schools की ट्यूटोरियल: [JavaScript String toLowerCase() Method](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
