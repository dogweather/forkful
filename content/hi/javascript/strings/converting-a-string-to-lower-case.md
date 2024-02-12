---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
aliases:
- /hi/javascript/converting-a-string-to-lower-case/
date:                  2024-01-20T17:38:48.521086-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JavaScript में किसी string को lower case में बदलना मतलब उस string के सभी अक्षरों को छोटे (small letters) में परिवर्तित करना है। ऐसा तब किया जाता है जब हमें case sensitivity से बचना होता है, जैसे यूजर इनपुट को सामान्यीकृत करना या डेटा की comparison और sorting करते समय।

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
