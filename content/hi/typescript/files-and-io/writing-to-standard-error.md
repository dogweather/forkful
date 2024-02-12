---
title:                "मानक त्रुटि के लिए लिखना"
date:                  2024-02-03T19:35:34.434242-07:00
model:                 gpt-4-0125-preview
simple_title:         "मानक त्रुटि के लिए लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
TypeScript में, मानक त्रुटि (stderr) में लिखना एक प्रक्रिया है जिसमें त्रुटि संदेशों या लॉगों को सीधे पर्यावरण के त्रुटि आउटपुट स्ट्रीम में भेजा जाता है (जैसे कि node.js या एक वेब ब्राउज़र में कंसोल)। यह कार्यक्रम डेटा के लिए आमतौर पर प्रयुक्त मानक आउटपुट (stdout) के साथ हस्तक्षेप किए बिना समस्याओं का निदान करने के लिए आवश्यक है, सुनिश्चित करता है कि त्रुटि हैंडलिंग और लॉगिंग कुशलता और सुसंगतता से प्रबंधित की जाती है।

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