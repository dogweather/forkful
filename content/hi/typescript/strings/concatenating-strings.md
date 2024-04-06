---
date: 2024-01-20 17:36:18.566942-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u0941\
  \u0930\u093E\u0928\u0947 \u091C\u092E\u093E\u0928\u0947 \u092E\u0947\u0902, strings\
  \ \u0915\u094B concatenation \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ \u092C\u0938 '+' operator \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\
  \u0932 \u0939\u094B\u0924\u093E \u0925\u093E\u0964 \u0932\u0947\u0915\u093F\u0928\
  \ \u0907\u0938\u092E\u0947\u0902 \u0915\u092D\u0940-\u0915\u092D\u0940 \u0917\u0921\
  \u093C\u092C\u0921\u093C \u0939\u094B \u091C\u093E\u0924\u0940 \u0925\u0940 \u091C\
  \u092C\u2026"
lastmod: '2024-04-05T22:51:06.547798-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u0941\u0930\u093E\
  \u0928\u0947 \u091C\u092E\u093E\u0928\u0947 \u092E\u0947\u0902, strings \u0915\u094B\
  \ concatenation \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092C\u0938\
  \ '+' operator \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0939\
  \u094B\u0924\u093E \u0925\u093E\u0964 \u0932\u0947\u0915\u093F\u0928 \u0907\u0938\
  \u092E\u0947\u0902 \u0915\u092D\u0940-\u0915\u092D\u0940 \u0917\u0921\u093C\u092C\
  \u0921\u093C \u0939\u094B \u091C\u093E\u0924\u0940 \u0925\u0940 \u091C\u092C numbers\
  \ \u0914\u0930 strings \u0915\u094B \u092E\u093F\u0932\u093E\u092F\u093E \u091C\u093E\
  \u0924\u093E \u0925\u093E\u0964 TypeScript \u091C\u0948\u0938\u0940 \u092E\u0949\
  \u0921\u0930\u094D\u0928 languages \u092E\u0947\u0902 template literals \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947, \u091C\
  \u093F\u0928\u094D\u0939\u0947\u0902 backticks (`) \u0938\u0947 \u092A\u0939\u091A\
  \u093E\u0928\u093E \u091C\u093E\u0924\u093E \u0939\u0948, \u0939\u092E \u0906\u0938\
  \u093E\u0928\u0940 \u0938\u0947 variables \u0915\u094B strings \u092E\u0947\u0902\
  \ \u0921\u093E\u0932 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902 \u0914\u0930 \u0909\
  \u0928\u094D\u0939\u0947\u0902 readable \u092C\u0928\u093E \u0938\u0915\u0924\u0947\
  \ \u0939\u0948\u0902\u0964 \u0907\u0938\u0938\u0947 \u0917\u0932\u0924\u093F\u092F\
  \u093E\u0901 \u0915\u092E \u0939\u094B\u0924\u0940 \u0939\u0948\u0902 \u0914\u0930\
  \ \u0915\u094B\u0921 \u0938\u093E\u092B-\u0938\u0941\u0925\u0930\u093E \u0930\u0939\
  \u0924\u093E \u0939\u0948\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091C\u094B\
  \u0921\u093C\u0928\u093E"
weight: 3
---

## How to: (कैसे करें:)
```TypeScript
// Simple String Concatenation with Plus (+) Operator
let greeting: string = "नमस्ते, ";
let name: string = "विश्व!";
let welcomeMessage: string = greeting + name;
console.log(welcomeMessage); // "नमस्ते, विश्व!"

// String Concatenation with Template Literals
let user: string = "अर्जुन";
let age: number = 30;
let introduction: string = `मेरा नाम ${user} है और मेरी उम्र ${age} वर्ष है।`;
console.log(introduction); // "मेरा नाम अर्जुन है और मेरी उम्र 30 वर्ष है।"
```

## Deep Dive (गहराई में जानकारी)
पुराने जमाने में, strings को concatenation करने के लिए बस '+' operator का इस्तेमाल होता था। लेकिन इसमें कभी-कभी गड़बड़ हो जाती थी जब numbers और strings को मिलाया जाता था। TypeScript जैसी मॉडर्न languages में template literals का इस्तेमाल करके, जिन्हें backticks (`) से पहचाना जाता है, हम आसानी से variables को strings में डाल सकते हैं और उन्हें readable बना सकते हैं। इससे गलतियाँ कम होती हैं और कोड साफ-सुथरा रहता है।

## See Also (और देखें)
- TypeScript Handbook on Template Literals: [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- MDN Web Docs on String Concatenation: [MDN String Concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat) 
- Stack Overflow discussion on String Concatenation in TypeScript: [Stack Overflow](https://stackoverflow.com/questions/tagged/typescript+string-concatenation)
