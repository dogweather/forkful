---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
html_title:           "Bash: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"

category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
लिखना एक टेक्स्ट फाइल में डेटा स्टोर करने का तरीका है. प्रोग्रामर्स लॉग्स, कॉन्फ़िगरेशन्स, डेटा एक्सपोर्ट करने के लिए इसका उपयोग करते हैं.

## How to: (कैसे करें:)
```TypeScript
import { writeFile } from 'fs';

// टेक्स्ट फाइल में लिखें
const data: string = 'Hello, this is a test';

writeFile('example.txt', data, (err) => {
  if (err) throw err;
  console.log('File written successfully!');
});
```

आउटपुट:
```
File written successfully!
```

## Deep Dive (गहराई में जानकारी)
टेक्स्ट फाइल्स को स्टोर करने का तरीका DOS और UNIX सिस्टम्स से है. बदलाव के लिए `appendFile`, या लाइब्रेरीज जैसे की `fs-extra` या `axios` का उपयोग कर सकते हैं. `fs` प्रॉमिस-बेस्ड वर्जन `fs.promises.writeFile` के साथ भी आता है जो async/await के साथ उपयोग किया जा सकता है.

## See Also (और भी देखें)
- Node.js `fs` मॉड्यूल डॉक्यूमेंटेशन: https://nodejs.org/api/fs.html
- `fs-extra` मॉड्यूल: https://www.npmjs.com/package/fs-extra
- `axios`: https://axios-http.com/docs/intro
