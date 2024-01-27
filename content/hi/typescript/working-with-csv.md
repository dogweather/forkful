---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV यानी Comma-Separated Values एक साधारण फाइल फॉर्मेट है जो डेटा को सरल स्वरूप में स्टोर करती है। प्रोग्रामर इसका इस्तेमाल डेटा को आसानी से आयात और निर्यात करने के लिए करते हैं क्योंकि यह हर तरह के प्रोग्राम्स और भाषाओं में आसानी से पढ़ा जा सकता है।

## How to: (कैसे करें:)
चलिए TypeScript में CSV फाइल को पढ़ने और लिखने का एक सिंपल उदाहरण देखते हैं।

```TypeScript
// CSV पढ़ने के लिए
import * as fs from 'fs';
import * as csv from 'fast-csv';

let filePath = 'data.csv';

fs.createReadStream(filePath)
  .pipe(csv.parse({ headers: true }))
  .on('data', (row) => console.log(row))
  .on('end', () => console.log('CSV पढ़ना पूरा हो गया!'));

// CSV लिखने के लिए
const writeStream = fs.createWriteStream('out.csv');
csv.write([
    { name: 'John', age: 27 },
    { name: 'Jane', age: 32 }
], { headers: true }).pipe(writeStream);
```

## Deep Dive (गहराई में जानकारी)
CSV प्रारूप का इतिहास 1970 के दशक तक जाता है, और यह डेटा के सरल विनिमय के लिए एक मानक माना जाता है। इसके विकल्पों में JSON, XML हैं, जो कि अधिक जटिल डेटा संरचना के लिए उपयोगी हैं। CSV के साथ काम करते समय, फाइल एनकोडिंग और डेटा सेनिटाइजेशन पर ध्यान देना महत्वपूर्ण है। `fast-csv` जैसी लाइब्रेरीज का इस्तेमाल करके हम इसे आसानी से कर सकते हैं।

## See Also (और भी देखें)
- [Node.js CSV parser (fast-csv)](https://c2fo.github.io/fast-csv/docs/introduction/getting-started)
- [MDN Web Docs - Working with text](https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications#Example.3A_Using_object_URLs_to_display_images)
- [npm: CSV package](https://www.npmjs.com/package/csv)
- [Stack Overflow CSV questions](https://stackoverflow.com/questions/tagged/csv)
