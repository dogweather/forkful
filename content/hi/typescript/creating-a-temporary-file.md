---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

सबसे पहले तो, अस्थायी फ़ाइल एक और कार्यवाही के लिए अश्लील डेटा को संग्रहीत करने का एक तरीका है, और यह केवल कुछ समय के लिए ही रहती है। प्रोग्रामर इसे इसलिए प्रयोग करते हैं, क्योंकि यह एक कामयाब तरीका है डेटा के नेतृत्व प्रवाह को हेंडल करने का, विशेषकर किसी शास्त्रीय कामचालन में।

## कैसे करें:

यहां TypeScript में अस्थायी फ़ाइल कैसे बनाई जाती है, इसका उदाहरण है:

```TypeScript
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';

function createTempFile(fileName: string): string {
    const tempDirectory = fs.mkdtempSync(path.join(os.tmpdir(), 'my-temp-files-'));
    const filePath = path.join(tempDirectory, fileName);

    fs.writeFileSync(filePath, 'Hello, world!');
    return filePath;
}

const tempFilePath = createTempFile('mytemp.txt');
console.log(`Temp file path: ${tempFilePath}`);
```

उपरोक्त कोड संपादन और चलाने के बाद, आपको कुछ ऐसा आउटपुट मिलेगा जैसा निम्नलिखित है:

```TypeScript
Temp file path: /tmp/my-temp-files-ujx3nv6z/mytemp.txt
```

## गहराई में अध्ययन

(1) हिस्टॉरिकल कॉंटेक्स्ट: अस्थायी फ़ाइलें कंप्यूटर साइंस के आरंभ से ही मौजूद थीं, आवश्यकता और कामचलन नियंत्रण को केंद्रीय रूप से रखते हुए।

(2) वैकल्पिक विचारधाराएं: अन्य भाषाओं में, आप अस्थायी फ़ाइल बनाने के लिए `tempfile` जैसे मॉड्यूल्स का उपयोग कर सकते हैं, जैसे कि Python में। TypeScript में, आप `os` मॉड्यूल का उपयोग करके अस्थायी फ़ाइलें बना सकते हैं।

(3) कार्यान्वयन विवरण: इस कोड स्निपेट में, हमने पहले `os.tmpdir()` का उपयोग किया है जो सिस्टम अस्थायी डायरेक्टरी का पथ लौटाता है। फिर हमने `fs.mkdtempSync` का उपयोग किया है जो एक अद्वितीय अस्थायी डायरेक्टरी बनाता है। अंतिम चरण में, हमने `fs.writeFileSync` का उपयोग किया है जो नई फ़ाइल में डेटा लिखता है।

## अधिक देखें

अधिक विवरण और सहायता के लिए, निम्नलिखित स्रोतों का उपयोग करें:

1. Node.js डॉक्यूमेंटेशन: https://nodejs.org/docs/latest-v15.x/api/fs.html
2. TypeScript डॉक्यूमेंटेशन: https://www.typescriptlang.org/docs/
3. StackOverflow: https://stackoverflow.com/questions/4482686/check-synchronously-if-file-directory-exists-in-node-js