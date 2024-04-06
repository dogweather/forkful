---
date: 2024-01-20 17:42:12.160437-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092F\u0939\
  \ \u0915\u094B\u0921 \u090F\u0915 \u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\
  \u093E\u0907\u0932 'temp.txt' \u092C\u0928\u093E\u0924\u093E \u0939\u0948 \u0914\
  \u0930 \u0909\u0938\u092E\u0947\u0902 \u0921\u0947\u091F\u093E \u0932\u093F\u0916\
  \u0924\u093E \u0939\u0948\u0964."
lastmod: '2024-04-05T21:53:53.928143-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092F\u0939 \u0915\
  \u094B\u0921 \u090F\u0915 \u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\
  \u0907\u0932 'temp.txt' \u092C\u0928\u093E\u0924\u093E \u0939\u0948 \u0914\u0930\
  \ \u0909\u0938\u092E\u0947\u0902 \u0921\u0947\u091F\u093E \u0932\u093F\u0916\u0924\
  \u093E \u0939\u0948\u0964."
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u092C\
  \u0928\u093E\u0928\u093E"
weight: 21
---

## How to: (कैसे करें:)
```TypeScript
import { writeFileSync, mkdtemp } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';

// अस्थायी डायरेक्टरी बनाना
mkdtemp(join(tmpdir(), 'my-app-'), (err, directory) => {
  if (err) throw err;

  // टेम्पररी फाइल का निर्माण करना और डेटा लिखना
  const tempFilePath = join(directory, 'temp.txt');
  writeFileSync(tempFilePath, 'यह एक अस्थायी डेटा है।');

  console.log(`टेम्पररी फाइल बन गयी: ${tempFilePath}`);
  // आउटपुट: टेम्पररी फाइल बन गयी: /tmp/my-app-XXXXXX/temp.txt
});
```
यह कोड एक अस्थायी फाइल 'temp.txt' बनाता है और उसमें डेटा लिखता है।

## Deep Dive (गहराई से जानना)
अस्थायी फाइलें प्रोग्राम्मिंग में स्थाई डेटा स्टोर करने की बजाय, अल्पकालिक डेटा स्टोरेज के तौर पर उपयोग होती हैं, जैसे कि सत्र डेटा या इंटरमीडिएट प्रोसेसिंग के लिए। इतिहास में, अस्थायी फाइलें मेमोरी के दबाव को कम करने और सिस्टम को स्थिर बनाने के लिए महत्वपूर्ण थी। अल्टरनेटिव आप्रोच में मेमोरी-बेस्ड बफर्स या डेटाबेसज हो सकते हैं। टेम्पररी फाइलों को बनाने के लिए, Node.js के `fs` मॉड्यूल के `mkdtemp` फंक्शन का इस्तेमाल होता है, यह एक यूनिक डायरेक्टरी नाम के साथ एक टेम्पररी डायरेक्टरी बनाता है।

## See Also (यह भी देखें)
- Node.js fs Documentation: [Node.js File System - fs](https://nodejs.org/api/fs.html)
- Understanding tmpdir in Node.js: [OS Temp Directory - os.tmpdir()](https://nodejs.org/api/os.html#ostmpdir)
