---
date: 2024-01-26 03:48:06.852016-07:00
description: "\u0915\u0948\u0938\u0947: TypeScript \u092E\u0947\u0902 \u0938\u0902\
  \u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u0917\u094B\u0932 \u0915\u0930\
  \u0928\u093E \u0915\u0908 \u0924\u0930\u0940\u0915\u094B\u0902 \u0938\u0947 \u0915\
  \u093F\u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964 \u092F\
  \u0939\u093E\u0901 \u090F\u0915 \u0924\u094D\u0935\u0930\u093F\u0924 \u0905\u0935\
  \u0932\u094B\u0915\u0928 \u0926\u093F\u092F\u093E \u0917\u092F\u093E \u0939\u0948\
  ."
lastmod: '2024-03-13T22:44:51.880456-06:00'
model: gpt-4-0125-preview
summary: "TypeScript \u092E\u0947\u0902 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\
  \u0902 \u0915\u094B \u0917\u094B\u0932 \u0915\u0930\u0928\u093E \u0915\u0908 \u0924\
  \u0930\u0940\u0915\u094B\u0902 \u0938\u0947 \u0915\u093F\u092F\u093E \u091C\u093E\
  \ \u0938\u0915\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915\
  \ \u0924\u094D\u0935\u0930\u093F\u0924 \u0905\u0935\u0932\u094B\u0915\u0928 \u0926\
  \u093F\u092F\u093E \u0917\u092F\u093E \u0939\u0948."
title: "\u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u092A\u0942\
  \u0930\u094D\u0923\u093E\u0902\u0915 \u092C\u0928\u093E\u0928\u093E"
weight: 13
---

## कैसे:
TypeScript में संख्याओं को गोल करना कई तरीकों से किया जा सकता है। यहाँ एक त्वरित अवलोकन दिया गया है:

```typescript
// Math.round सबसे निकटतम पूर्णांक तक गोल करता है
console.log(Math.round(1.5)); // आउटपुट: 2

// Math.ceil सबसे निकटतम पूर्णांक तक ऊपर की ओर गोल करता है
console.log(Math.ceil(1.1)); // आउटपुट: 2

// Math.floor सबसे निकटतम पूर्णांक तक नीचे की ओर गोल करता है
console.log(Math.floor(1.8)); // आउटपुट: 1

// toFixed एक निश्चित संख्या में दशमलव स्थानों तक गोल करता है
let num = 1.23456;
console.log(num.toFixed(2)); // आउटपुट: "1.23"
// ध्यान दें: toFixed एक स्ट्रिंग वापस करता है! आवश्यकता होने पर वापस परिवर्तित करने के लिए parseFloat का उपयोग करें।
console.log(parseFloat(num.toFixed(2))); // आउटपुट: 1.23

```

## गहराई में
पहले के दिनों में, सीमित स्थान और प्रिसिजन मुद्दों के कारण संख्याओं को गोल करना आवश्यक था। आज, फ्लोटिंग-पॉइंट अंकगणित बाइनरी में संख्याओं को कैसे संग्रहीत किया जाता है, इसके कारण अजीब परिणामों को प्राप्त कर सकता है। गोल करने के विकल्प में फ्लोर, सील, और ट्रंक (बिना गोल किए दशमलव को हटाने के लिए) शामिल हैं।

आंतरिक महत्व हैं: `Math.round` "राउंड हाफ अप" (जिसे "कमर्शियल राउंडिंग" भी कहा जाता है) का अनुसरण करता है, जबकि `Math.floor` और `Math.ceil` सीधे होते हैं। `toFixed` अप्रत्याशित परिणाम पैदा कर सकता है क्योंकि यह एक स्ट्रिंग लौटाता है, और यह "राउंड हाफ टू ईवन" (जिसे "बैंकर्स राउंडिंग" भी कहा जाता है) का उपयोग करता है, विशेषकर एक ही संख्याओं को कई बार गोल करने में पूर्वाग्रह को कम करने के लिए उपयोगी है।

## देखें
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE मानक फ्लोटिंग-पॉइंट अंकगणित के लिए (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
