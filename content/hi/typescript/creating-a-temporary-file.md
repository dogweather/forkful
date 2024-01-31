---
title:                "अस्थायी फाइल बनाना"
date:                  2024-01-20T17:42:12.160437-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेम्पररी फाइल एक ऐसी फाइल होती है जिसे अस्थायी डेटा के लिए बनाया जाता है। प्रोग्रामर्स इसे इसलिए उपयोग करते हैं क्योंकि यह अस्थायी प्रोसेसिंग, कैशिंग, या बैकअप के लिए एक सुरक्षित जगह प्रदान करती है।

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
