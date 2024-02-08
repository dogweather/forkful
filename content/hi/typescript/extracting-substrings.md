---
title:                "सबस्ट्रिंग्स निकालना"
aliases:
- hi/typescript/extracting-substrings.md
date:                  2024-01-20T17:47:29.542402-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
सबस्ट्रिंग्स निकालने का मतलब है किसी स्ट्रिंग के एक हिस्से को चुनना और उसे अलग से प्रस्तुत करना। प्रोग्रामर्स डेटा को संसाधित करने, सर्च करने और संवाद करने के लिए इस प्रक्रिया का उपयोग करते हैं।

## How to (कैसे करें):
```TypeScript
let message: string = 'नमस्ते TypeScript!';

// पहले 7 कैरेक्टर्स निकालें
let greeting: string = message.substring(0, 7);
console.log(greeting); // नमस्ते 

// 8वें कैरेक्टर से आखिर तक निकालें
let topic: string = message.substring(8);
console.log(topic); // TypeScript!
```

यहाँ `substring` फंक्शन का उपयोग हो रहा है, जिसमें पहला पैरामीटर शुरुआती इंडेक्स है, और दूसरा अंतिम इंडेक्स (अनिवार्य नहीं) है।

## Deep Dive (गहराई से जानकारी):
सबस्ट्रिंग की अवधारणा शुरुआत से ही प्रोग्रामिंग में रही है जब से स्ट्रिंग्स को संभालना शुरू किया गया था। यह डेटा संरचना और एल्गोरिदम की बुनियाद है। सबस्ट्रिंग्स को निकालने के विकल्प में `slice`, `substr` (जिसे TypeScript में हटा दिया गया है), और `substring` आते हैं। `substring` और `slice` में मुख्य अंतर यह है कि `slice` में नकारात्मक इंडेक्सेस स्वीकार्य हैं, जबकि `substring` में नहीं। इसके अलावा, यदि `substring` को बड़े इंडेक्स से शुरू किया जाता है तो यह खुद को स्विच कर लेता है ताकि छोटे इंडेक्स से शुरुआत हो।

## See Also (और जानकारी के लिंक्स):
- TypeScript Handbook (टाइपस्क्रिप्ट हैंडबुक): [TypeScript Handbook - Strings](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#strings)
- MDN Web Docs (MDN वेब डॉक्स): [String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
