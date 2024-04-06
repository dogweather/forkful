---
date: 2024-01-20 18:05:07.580200-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: TypeScript \u0935\
  \u093E\u0938\u094D\u0924\u0935 \u092E\u0947\u0902 JavaScript \u0915\u093E \u090F\
  \u0915 \u0938\u0941\u0927\u093E\u0930 \u0939\u0948, \u091C\u093F\u0938\u0947 \u092E\
  \u093E\u0907\u0915\u094D\u0930\u094B\u0938\u0949\u092B\u094D\u091F \u0928\u0947\
  \ 2012 \u092E\u0947\u0902 \u0932\u093E\u0902\u091A \u0915\u093F\u092F\u093E \u0925\
  \u093E\u0964 \u0907\u0938\u092E\u0947\u0902 static typing \u0914\u0930 class-based\
  \ objects \u091C\u0948\u0938\u0947\u2026"
lastmod: '2024-04-05T22:51:06.560486-06:00'
model: gpt-4-1106-preview
summary: "TypeScript \u0935\u093E\u0938\u094D\u0924\u0935 \u092E\u0947\u0902 JavaScript\
  \ \u0915\u093E \u090F\u0915 \u0938\u0941\u0927\u093E\u0930 \u0939\u0948, \u091C\u093F\
  \u0938\u0947 \u092E\u093E\u0907\u0915\u094D\u0930\u094B\u0938\u0949\u092B\u094D\u091F\
  \ \u0928\u0947 2012 \u092E\u0947\u0902 \u0932\u093E\u0902\u091A \u0915\u093F\u092F\
  \u093E \u0925\u093E\u0964 \u0907\u0938\u092E\u0947\u0902 static typing \u0914\u0930\
  \ class-based objects \u091C\u0948\u0938\u0947 features \u0939\u094B\u0924\u0947\
  \ \u0939\u0948\u0902 \u091C\u093F\u0938\u0938\u0947 large-scale applications \u0915\
  \u094B \u0938\u0902\u092D\u093E\u0932\u0928\u093E \u0906\u0938\u093E\u0928 \u0939\
  \u094B \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u0928\u090F \u092A\u094D\u0930\
  \u094B\u091C\u0947\u0915\u094D\u091F\u094D\u0938 \u092E\u0947\u0902 TypeScript \u0915\
  \u093E \u091A\u092F\u0928 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0915\u0908 alternatives\
  \ \u0939\u094B\u0924\u0947 \u0939\u0948\u0902, \u091C\u0948\u0938\u0947 \u0915\u093F\
  \ pure JavaScript, Flow, \u092F\u093E Dart \u0932\u0947\u0915\u093F\u0928 TypeScript\
  \ \u0905\u092A\u0928\u0947 robust tools \u0914\u0930 community support \u0915\u0947\
  \ \u0915\u093E\u0930\u0923 \u0906\u091C\u0915\u0932 \u0915\u093E\u092B\u0940 \u0932\
  \u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0939\u0948\u0964 \u0938\u094D\u091F\
  \u0948\u091F\u093F\u0915 \u091F\u093E\u0907\u092A\u093F\u0902\u0917 \u0915\u0940\
  \ \u092E\u0926\u0926 \u0938\u0947, \u092C\u0917\u094D\u0938 \u0915\u0940 \u092A\u0939\
  \u091A\u093E\u0928 \u092A\u0939\u0932\u0947 \u0939\u0940 \u0939\u094B \u091C\u093E\
  \u0924\u0940 \u0939\u0948 \u0914\u0930 code \u0915\u094B maintain \u0915\u0930\u0928\
  \u093E \u0906\u0938\u093E\u0928 \u0939\u094B \u091C\u093E\u0924\u093E \u0939\u0948\
  \u0964."
title: "\u0928\u0908 \u092A\u0930\u093F\u092F\u094B\u091C\u0928\u093E \u0936\u0941\
  \u0930\u0942 \u0915\u0930\u0928\u093E"
weight: 1
---

## कैसे करें:
```TypeScript
// TypeScript के साथ नया प्रोजेक्ट शुरू करना

// 1. npm का उपयोग कर एक नयी TypeScript प्रोजेक्ट तैयार करें:
$ npm init -y
$ npm install typescript --save-dev

// 2. TypeScript configuration file (tsconfig.json) बनाएँ:
$ npx tsc --init

// 3. एक साधारण "Hello, World" उदाहरण लिखें:
const greeting: string = "Hello, World";
console.log(greeting);

// 4. TypeScript को javascript में transpile करें और चलाएँ:
$ npx tsc
$ node .\dist\app.js
```

सैम्पल आउटपुट:
```
Hello, World
```

## गहराई से जानिए:
TypeScript वास्तव में JavaScript का एक सुधार है, जिसे माइक्रोसॉफ्ट ने 2012 में लांच किया था। इसमें static typing और class-based objects जैसे features होते हैं जिससे large-scale applications को संभालना आसान हो जाता है। नए प्रोजेक्ट्स में TypeScript का चयन करने के कई alternatives होते हैं, जैसे कि pure JavaScript, Flow, या Dart लेकिन TypeScript अपने robust tools और community support के कारण आजकल काफी लोकप्रिय है। स्टैटिक टाइपिंग की मदद से, बग्स की पहचान पहले ही हो जाती है और code को maintain करना आसान हो जाता है।

## और भी देखें:
- TypeScript हैंडबुक: https://www.typescriptlang.org/docs/handbook/intro.html
- TypeScript GitHub repository: https://github.com/Microsoft/TypeScript
- npm (Node Package Manager) गाइड: https://docs.npmjs.com/
- Node.js: https://nodejs.org/en/
