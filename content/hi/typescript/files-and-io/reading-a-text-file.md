---
date: 2024-01-20 17:56:10.900073-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) TypeScript\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947 \u091F\
  \u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932 \u092A\u0922\
  \u093C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `fs` \u092E\u0949\u0921\u094D\
  \u092F\u0942\u0932 \u0915\u093E \u092A\u094D\u0930\u092F\u094B\u0917 \u0939\u094B\
  \u0924\u093E \u0939\u0948\u0964 \u0928\u093F\u091A\u0947 \u0915\u094B\u0921 \u0914\
  \u0930 \u0909\u0924\u094D\u092A\u093E\u0926\u0928 \u0915\u093E \u0909\u0926\u093E\
  \u0939\u0930\u0923 \u0926\u093F\u092F\u093E \u0917\u092F\u093E \u0939\u0948."
lastmod: '2024-04-05T21:53:53.924495-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) TypeScript \u0907\u0938\
  \u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947 \u091F\u0947\u0915\
  \u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932 \u092A\u0922\u093C\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F `fs` \u092E\u0949\u0921\u094D\u092F\u0942\
  \u0932 \u0915\u093E \u092A\u094D\u0930\u092F\u094B\u0917 \u0939\u094B\u0924\u093E\
  \ \u0939\u0948\u0964 \u0928\u093F\u091A\u0947 \u0915\u094B\u0921 \u0914\u0930 \u0909\
  \u0924\u094D\u092A\u093E\u0926\u0928 \u0915\u093E \u0909\u0926\u093E\u0939\u0930\
  \u0923 \u0926\u093F\u092F\u093E \u0917\u092F\u093E \u0939\u0948."
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
weight: 22
---

## How to: (कैसे करें:)
TypeScript इस्तेमाल करके टेक्स्ट फ़ाइल पढ़ने के लिए `fs` मॉड्यूल का प्रयोग होता है। निचे कोड और उत्पादन का उदाहरण दिया गया है:

```typescript
import fs from 'fs';
import { promisify } from 'util';

const readFile = promisify(fs.readFile);

async function readTextFile(filePath: string): Promise<string> {
  try {
    const data = await readFile(filePath, 'utf-8');
    console.log(data);
    return data;
  } catch (error) {
    console.error('Error reading file:', error);
    throw error;
  }
}

// इस्तेमाल करें इस फंक्शन का:
readTextFile('./example.txt')
  .then(content => console.log('File content:', content))
  .catch(err => console.error(err));
```

संभावित उत्पादन:
```
Hello, यह एक उदाहरण टेक्स्ट है।
File content: Hello, यह एक उदाहरण टेक्स्ट है।
```

## Deep Dive (गहराई में जानकारी):
पहले, प्रोग्रामर `fs` मॉड्यूल का `readFileSync` फंक्शन इस्तेमाल करते थे, जो ब्लॉकिंग था - यानि पूरे कोड को रोक देता था जब तक फ़ाइल पूरी पढ़ी नहीं जाती थी। अब प्रोमिसेज़ और async/await पैटर्न इस्तेमाल होते हैं ताकि I/O ऑपरेशन्स नॉन-ब्लॉकिंग हों और बेहतर प्रदर्शन के लिए। विकल्प के तौर पर, `readFile` के स्ट्रीमिंग वर्जन `createReadStream` का भी प्रयोग होता है जब बड़ी फ़ाइल्स को पढ़ना हो।

## See Also (इसे भी देखें):
- Node.js डॉक्यूमेंटेशन `fs` मॉड्यूल पर: [Node.js 'fs' Documentation](https://nodejs.org/api/fs.html)
- `async/await` की गाइड: [Understanding async/await in JavaScript](https://javascript.info/async-await)
- फ़ाइल सिस्टम एक्सेस API के बारे में: [The File System Access API: Simplifying access to local files](https://web.dev/file-system-access/)
