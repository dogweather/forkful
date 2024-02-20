---
date: 2024-01-20 17:36:18.566942-07:00
description: "String concatenation \u0926\u094B \u092F\u093E \u0926\u094B \u0938\u0947\
  \ \u0905\u0927\u093F\u0915 strings \u0915\u094B \u091C\u094B\u0921\u093C\u0928\u0947\
  \ \u0915\u093E \u0915\u093E\u092E \u0939\u0948\u0964 \u0907\u0938 \u0915\u093E\u092E\
  \ \u0915\u094B \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\
  \u0938 \u0905\u0915\u094D\u0938\u0930 \u0921\u0947\u091F\u093E \u0915\u094B format\
  \ \u0915\u0930\u0928\u0947 \u092F\u093E \u092E\u0947\u0938\u0947\u091C\u0947\u0938\
  \ \u0915\u094D\u0930\u093F\u090F\u091F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
lastmod: 2024-02-19 22:05:10.863261
model: gpt-4-1106-preview
summary: "String concatenation \u0926\u094B \u092F\u093E \u0926\u094B \u0938\u0947\
  \ \u0905\u0927\u093F\u0915 strings \u0915\u094B \u091C\u094B\u0921\u093C\u0928\u0947\
  \ \u0915\u093E \u0915\u093E\u092E \u0939\u0948\u0964 \u0907\u0938 \u0915\u093E\u092E\
  \ \u0915\u094B \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\
  \u0938 \u0905\u0915\u094D\u0938\u0930 \u0921\u0947\u091F\u093E \u0915\u094B format\
  \ \u0915\u0930\u0928\u0947 \u092F\u093E \u092E\u0947\u0938\u0947\u091C\u0947\u0938\
  \ \u0915\u094D\u0930\u093F\u090F\u091F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091C\u094B\
  \u0921\u093C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
String concatenation दो या दो से अधिक strings को जोड़ने का काम है। इस काम को प्रोग्रामर्स अक्सर डेटा को format करने या मेसेजेस क्रिएट करने के लिए करते हैं।

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
