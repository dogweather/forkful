---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:36.069168-07:00
description: "\u0915\u0948\u0938\u0947: Node.js \u092E\u0947\u0902, stderr \u092A\u0930\
  \ \u0932\u093F\u0916\u0928\u093E `console.error()` \u092E\u0947\u0925\u0921 \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u092F\u093E \u0938\
  \u0940\u0927\u0947 `process.stderr` \u092A\u0930 \u0932\u093F\u0916\u0915\u0930\
  \ \u092A\u0942\u0930\u093E \u0915\u093F\u092F\u093E \u091C\u093E \u0938\u0915\u0924\
  \u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u0926\u094B\u0928\u094B\u0902\
  \ \u0926\u0943\u0937\u094D\u091F\u093F\u0915\u094B\u0923\u094B\u0902 \u0915\u093E\
  \u2026"
lastmod: '2024-03-13T22:44:53.017906-06:00'
model: gpt-4-0125-preview
summary: "Node.js \u092E\u0947\u0902, stderr \u092A\u0930 \u0932\u093F\u0916\u0928\
  \u093E `console.error()` \u092E\u0947\u0925\u0921 \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930\u0915\u0947 \u092F\u093E \u0938\u0940\u0927\u0947 `process.stderr`\
  \ \u092A\u0930 \u0932\u093F\u0916\u0915\u0930 \u092A\u0942\u0930\u093E \u0915\u093F\
  \u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\
  \u093E\u0901 \u0926\u094B\u0928\u094B\u0902 \u0926\u0943\u0937\u094D\u091F\u093F\
  \u0915\u094B\u0923\u094B\u0902 \u0915\u093E \u0909\u0926\u093E\u0939\u0930\u0923\
  \ \u0926\u093F\u0916\u093E\u092F\u093E \u0917\u092F\u093E \u0939\u0948."
title: "\u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0915\u0947\
  \ \u0932\u093F\u090F \u0932\u093F\u0916\u0928\u093E"
weight: 25
---

## कैसे:
Node.js में, stderr पर लिखना `console.error()` मेथड का उपयोग करके या सीधे `process.stderr` पर लिखकर पूरा किया जा सकता है। यहाँ दोनों दृष्टिकोणों का उदाहरण दिखाया गया है:

```javascript
// console.error() का उपयोग करते हुए
console.error('यह एक त्रुटि संदेश है।');

// सीधे process.stderr पर लिखना
process.stderr.write('यह एक और त्रुटि संदेश है।\n');
```

दोनों विधियों के लिए नमूना आउटपुट stderr स्ट्रीम में प्रकट होगा, stdout के साथ मिलान नहीं करेगा:
```
यह एक त्रुटि संदेश है।
यह एक और त्रुटि संदेश है।
```

अधिक सुव्यवस्थित या एप्लिकेशन-विशिष्ट लॉगिंग के लिए, बहुत से JavaScript प्रोग्रामर `winston` या `bunyan` जैसी थर्ड-पार्टी लाइब्रेरियों का उपयोग करते हैं। यहाँ `winston` का उपयोग करते हुए एक त्वरित उदाहरण है:

पहले, npm के माध्यम से `winston` को इंस्टाल करें:
```shell
npm install winston
```

फिर, stderr पर त्रुटियों को लॉग करने के लिए `winston` को कॉन्फ़िगर करें:
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// एक त्रुटि संदेश लॉग करना
logger.error('winston के माध्यम से लॉग की गई त्रुटि।');
```

यह सेटअप सुनिश्चित करता है कि जब आप `winston` का उपयोग करके एक त्रुटि को लॉग करते हैं, तो यह stderr में निर्देशित होती है, जिससे सामान्य और त्रुटि आउटपुट्स के बीच स्पष्ट विभाजन बना रहता है।
