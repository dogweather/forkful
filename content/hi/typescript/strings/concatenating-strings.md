---
date: 2024-01-20 17:36:18.566942-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:51.874498-06:00'
model: gpt-4-1106-preview
summary: .
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
