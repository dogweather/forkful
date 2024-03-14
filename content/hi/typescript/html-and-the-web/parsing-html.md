---
date: 2024-01-20 15:35:07.424069-07:00
description: "HTML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947 \u0915\u093E\
  \ \u092E\u0924\u0932\u092C \u0939\u0948 HTML \u0921\u093E\u091F\u093E \u0915\u094B\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u092D\
  \u093E\u0937\u093E \u092E\u0947\u0902 \u0938\u092E\u091D\u0928\u0947 \u092F\u094B\
  \u0917\u094D\u092F \u0930\u0942\u092A \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\
  \u093E\u0964 \u0907\u0938\u0947 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E\
  \ \u0939\u0948 \u0924\u093E\u0915\u093F \u0935\u0947\u092C \u092A\u0947\u091C \u0915\
  \u0940 \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u0915\u094B \u092A\u0922\u093C\
  \u093E \u092F\u093E \u0938\u0902\u0936\u094B\u0927\u093F\u0924 \u0915\u093F\u092F\
  \u093E\u2026"
lastmod: '2024-03-13T22:44:51.885266-06:00'
model: unknown
summary: "HTML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947 \u0915\u093E\
  \ \u092E\u0924\u0932\u092C \u0939\u0948 HTML \u0921\u093E\u091F\u093E \u0915\u094B\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u092D\
  \u093E\u0937\u093E \u092E\u0947\u0902 \u0938\u092E\u091D\u0928\u0947 \u092F\u094B\
  \u0917\u094D\u092F \u0930\u0942\u092A \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\
  \u093E\u0964 \u0907\u0938\u0947 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E\
  \ \u0939\u0948 \u0924\u093E\u0915\u093F \u0935\u0947\u092C \u092A\u0947\u091C \u0915\
  \u0940 \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u0915\u094B \u092A\u0922\u093C\
  \u093E \u092F\u093E \u0938\u0902\u0936\u094B\u0927\u093F\u0924 \u0915\u093F\u092F\
  \u093E\u2026"
title: "HTML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
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
