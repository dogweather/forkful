---
title:                "टेक्स्ट फ़ाइल पढ़ना"
aliases: - /hi/typescript/reading-a-text-file.md
date:                  2024-01-20T17:56:10.900073-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेक्स्ट फ़ाइल पढ़ना मतलब फ़ाइल से डाटा निकालना है। प्रोग्रामर्स यह इसलिए करते हैं क्योंकि अक्सर सेटिंग्स, कॉन्फ़िगुरेशन या यूज़र डाटा टेक्स्ट फ़ाइल्स में होता है।

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
