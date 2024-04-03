---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:35:34.434242-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: TypeScript, \u091C\
  \u094B \u0915\u093F JavaScript \u0915\u093E \u090F\u0915 \u0938\u0941\u092A\u0930\
  \u0938\u0947\u091F \u0939\u0948, stderr \u092E\u0947\u0902 \u0932\u093F\u0916\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F \u0905\u0902\u0924\u0930\u094D\u0928\u093F\
  \u0939\u093F\u0924 JS \u0930\u0928\u091F\u093E\u0907\u092E \u092A\u0930\u094D\u092F\
  \u093E\u0935\u0930\u0923 (\u091C\u0948\u0938\u0947 Node.js) \u092A\u0930 \u0928\u093F\
  \u0930\u094D\u092D\u0930 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\
  \u093E\u0902\u2026"
lastmod: '2024-03-13T22:44:51.917422-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, \u091C\u094B \u0915\u093F JavaScript \u0915\u093E \u090F\u0915\
  \ \u0938\u0941\u092A\u0930\u0938\u0947\u091F \u0939\u0948, stderr \u092E\u0947\u0902\
  \ \u0932\u093F\u0916\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0905\u0902\u0924\
  \u0930\u094D\u0928\u093F\u0939\u093F\u0924 JS \u0930\u0928\u091F\u093E\u0907\u092E\
  \ \u092A\u0930\u094D\u092F\u093E\u0935\u0930\u0923 (\u091C\u0948\u0938\u0947 Node.js)\
  \ \u092A\u0930 \u0928\u093F\u0930\u094D\u092D\u0930 \u0915\u0930\u0924\u093E \u0939\
  \u0948\u0964 \u092F\u0939\u093E\u0902 \u092C\u0924\u093E\u092F\u093E \u0917\u092F\
  \u093E \u0939\u0948 \u0915\u093F \u0906\u092A \u0907\u0938\u0947 \u0938\u0940\u0927\
  \u0947 \u0915\u0948\u0938\u0947 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\
  \u0902."
title: "\u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0915\u0947\
  \ \u0932\u093F\u090F \u0932\u093F\u0916\u0928\u093E"
weight: 25
---

## कैसे करें:
TypeScript, जो कि JavaScript का एक सुपरसेट है, stderr में लिखने के लिए अंतर्निहित JS रनटाइम पर्यावरण (जैसे Node.js) पर निर्भर करता है। यहां बताया गया है कि आप इसे सीधे कैसे कर सकते हैं:

```typescript
console.error("This is an error message.");
```

stderr पर नमूना आउटपुट:
```
This is an error message.
```

Node.js पर्यावरण में, आप `process.stderr.write()` मेथड का उपयोग करके और अधिक निम्न स्तरीय लेखन कर सकते हैं:

```typescript
process.stderr.write("Low level error message.\n");
```

stderr पर नमूना आउटपुट:
```
Low level error message.
```

अधिक संरचित त्रुटि लॉगिंग के लिए, आप `winston` या `pino` जैसे लोकप्रिय तृतीय-पक्ष लाइब्रेरीज का उपयोग कर सकते हैं। `winston` का उपयोग करके त्रुटियों को लॉग करने का तरीका यहाँ दिया गया है:

पहले, `winston` को स्थापित करें:

```bash
npm install winston
```

फिर आपकी TypeScript फ़ाइल में इसका उपयोग करें:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('Error logged using winston.');
```

यह कंसोल और `error.log` नामक एक फाइल में त्रुटि लिखेगा। याद रखें, फाइलों में लेखन के समय, डिस्क स्पेस के उपयोग से संबंधित समस्याओं को रोकने के लिए फाइल अनुमतियों और रोलओवर का प्रबंधन करना महत्वपूर्ण है।
