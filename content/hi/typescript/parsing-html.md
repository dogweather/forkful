---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:35:07.424069-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

HTML पार्स करने का मतलब है HTML डाटा को प्रोग्रामिंग भाषा में समझने योग्य रूप में बदलना। इसे किया जाता है ताकि वेब पेज की सामग्री को पढ़ा या संशोधित किया जा सके।

## How to: (कैसे करें:)

आप TypeScript में HTML को पार्स करने के लिए `cheerio` जैसे लाइब्रेरी का उपयोग कर सकते हैं। यहां एक साधारण उदाहरण है:

```typescript
// npm install cheerio

import * as cheerio from 'cheerio';

const html = `<html>
  <head>
    <title>मेरा वेबपेज</title>
  </head>
  <body>
    <h1>नमस्ते दुनिया!</h1>
  </body>
</html>`;

const $ = cheerio.load(html);

console.log($('title').text());  // आउटपुट: मेरा वेबपेज
console.log($('h1').text());    // आउटपुट: नमस्ते दुनिया!
```

## Deep Dive (गहराई में जानकारी):

HTML पार्सिंग का इतिहास ब्राउजर के इवोल्यूशन के साथ ही शुरू होता है। DOM (Document Object Model) के आधार पर काम करने वाली लाइब्रेरीज by जैसे कि `cheerio`, `jsdom`, और `node-html-parser` वर्तमान में बहुत प्रचलित हैं। ये Node.js में HTML/XML डॉक्युमेंट्स को आसानी से पार्स और मैनिपुलेट करने के लिए बनाई गई हैं। इसका मुख्य काम वेब स्क्रेपिंग, टेस्टिंग, और सर्वर-साइड डॉम मैनिपुलेशन है।

## See Also (इसे भी देखें):

- Cheerio GitHub Repository: [Cheerio](https://github.com/cheeriojs/cheerio)
- Jsdom GitHub Repository: [Jsdom](https://github.com/jsdom/jsdom)
- Types for Node.js: [@types/node](https://www.npmjs.com/package/@types/node)
- TypeScript Documentation: [TypeScript](https://www.typescriptlang.org/docs/)
