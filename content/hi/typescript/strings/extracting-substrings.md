---
date: 2024-01-20 17:47:29.542402-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): \u092F\u0939\
  \u093E\u0901 `substring` \u092B\u0902\u0915\u094D\u0936\u0928 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0939\u094B \u0930\u0939\u093E \u0939\u0948, \u091C\u093F\
  \u0938\u092E\u0947\u0902 \u092A\u0939\u0932\u093E \u092A\u0948\u0930\u093E\u092E\
  \u0940\u091F\u0930 \u0936\u0941\u0930\u0941\u0906\u0924\u0940 \u0907\u0902\u0921\
  \u0947\u0915\u094D\u0938 \u0939\u0948, \u0914\u0930 \u0926\u0942\u0938\u0930\u093E\
  \ \u0905\u0902\u0924\u093F\u092E \u0907\u0902\u0921\u0947\u0915\u094D\u0938 (\u0905\
  \u0928\u093F\u0935\u093E\u0930\u094D\u092F \u0928\u0939\u0940\u0902) \u0939\u0948\
  \u0964."
lastmod: '2024-04-05T21:53:53.879688-06:00'
model: gpt-4-1106-preview
summary: "\u092F\u0939\u093E\u0901 `substring` \u092B\u0902\u0915\u094D\u0936\u0928\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0939\u094B \u0930\u0939\u093E \u0939\
  \u0948, \u091C\u093F\u0938\u092E\u0947\u0902 \u092A\u0939\u0932\u093E \u092A\u0948\
  \u0930\u093E\u092E\u0940\u091F\u0930 \u0936\u0941\u0930\u0941\u0906\u0924\u0940\
  \ \u0907\u0902\u0921\u0947\u0915\u094D\u0938 \u0939\u0948, \u0914\u0930 \u0926\u0942\
  \u0938\u0930\u093E \u0905\u0902\u0924\u093F\u092E \u0907\u0902\u0921\u0947\u0915\
  \u094D\u0938 (\u0905\u0928\u093F\u0935\u093E\u0930\u094D\u092F \u0928\u0939\u0940\
  \u0902) \u0939\u0948\u0964."
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

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
