---
title:                "अस्थायी फाइल बनाना"
aliases:
- hi/javascript/creating-a-temporary-file.md
date:                  2024-01-20T17:41:23.165071-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेम्परेरी फ़ाइल वह फ़ाइल होती है जिसे कोड चलते वक़्त अस्थायी रूप से बनाया जाता है। प्रोग्रामर इसे डेटा प्रोसेसिंग, लॉगिंग, या बड़े फ़ाइल ऑपरेशन्स के लिए उपयोग करते हैं।

## How to: (कैसे करें:)
```javascript
const fs = require('fs');
const os = require('os');
const path = require('path');

// टेम्परेरी फ़ाइल बनाने का फंक्शन
function createTempFile(content) {
  // टेम्परेरी डाइरेक्टरी पथ
  const tempDir = os.tmpdir();
  // टेम्परेरी फ़ाइल पथ बनाएं
  const tempFilePath = path.join(tempDir, `temp-${Date.now()}.txt`);
  // फ़ाइल बनाएं और डेटा लिखें
  fs.writeFileSync(tempFilePath, content);
  // फ़ाइल पथ वापस लौटाएं
  return tempFilePath;
}

// सेंपल कंटेंट
const sampleContent = 'यह एक उदाहरण है।';

// टेम्परेरी फ़ाइल बनाएं और पथ प्रिंट करें
const tempFile = createTempFile(sampleContent);
console.log(`टेम्परेरी फ़ाइल बन गई: ${tempFile}`);

// फ़ाइल पढ़ें
const readContent = fs.readFileSync(tempFile, 'utf8');
console.log(`फ़ाइल में क्या है: ${readContent}`);
```
सेम्पल आउटपुट:
```
टेम्परेरी फ़ाइल बन गई: C:\Users\...\AppData\Local\Temp\temp-1615406596776.txt
फ़ाइल में क्या है: यह एक उदाहरण है।
```

## Deep Dive (गहन जानकारी)
टेम्परेरी फ़ाइलें उस समय से इस्तेमाल हो रही हैं जब से कंप्यूटरों में मल्टीटास्किंग होने लगी। ये फ़ाइलें सिस्टम पर कम से कम समय के लिए डेटा स्टोर करती हैं और परफॉरमेंस बढ़ाने में मदद करती हैं। विकल्प के रूप में, आपके पास डेटाबेस, इन-मेमोरी स्टोरेज, या रिमोट सर्वर स्टोरेज हो सकते हैं। जावास्क्रिप्ट में, `fs` और `os` मॉड्यूल टेम्परेरी फ़ाइल बनाने की कमान संभालते हैं, जहाँ `fs` फाइल सिस्टम ऑपरेशन्स और `os` सिस्टम-लेवल जानकारी देता है।

## See Also (यह भी देखें)
- Node.js `fs` मॉड्यूल डॉक्यूमेंटेशन: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- Node.js `os` मॉड्यूल डॉक्यूमेंटेशन: [https://nodejs.org/api/os.html](https://nodejs.org/api/os.html)
- `path` मॉड्यूल का इस्तेमाल: [https://nodejs.org/api/path.html](https://nodejs.org/api/path.html)
