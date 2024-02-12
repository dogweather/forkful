---
title:                "नई परियोजना शुरू करना"
aliases:
- hi/typescript/starting-a-new-project.md
date:                  2024-01-20T18:05:07.580200-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

नया प्रोजेक्ट आरंभ करना मतलब है खाली जगह से कुछ अनोखा बनाने का कार्य शुरू करना। प्रोग्रामर नये प्रोजेक्ट्स पर काम करते हैं ताकि वे नए समाधान खोज सकें और अपनी सृजनात्मकता को व्यक्त कर सकें।

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
