---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:22.383905-07:00
description: "\u0915\u0948\u0938\u0947: TypeScript \u092E\u0947\u0902, \u0906\u092A\
  \ \u092E\u0942\u0932 \u0915\u094B\u0921 \u0915\u0947 \u092E\u093E\u0927\u094D\u092F\
  \u092E \u0938\u0947 \u092F\u093E `csv-parser` \u091C\u0948\u0938\u0947 \u0924\u0943\
  \u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092A\u0941\u0938\u094D\u0924\u0915\
  \u093E\u0932\u092F\u094B\u0902 \u0915\u093E \u0932\u093E\u092D \u0909\u0920\u093E\
  \u0915\u0930 CSV \u092B\u093E\u0907\u0932\u094B\u0902 \u0915\u0947 \u0938\u093E\u0925\
  \ \u0915\u093E\u092E \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902 \u091C\
  \u094B \u092A\u0922\u093C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\u2026"
lastmod: '2024-03-13T22:44:51.927609-06:00'
model: gpt-4-0125-preview
summary: "TypeScript \u092E\u0947\u0902, \u0906\u092A \u092E\u0942\u0932 \u0915\u094B\
  \u0921 \u0915\u0947 \u092E\u093E\u0927\u094D\u092F\u092E \u0938\u0947 \u092F\u093E\
  \ `csv-parser` \u091C\u0948\u0938\u0947 \u0924\u0943\u0924\u0940\u092F-\u092A\u0915\
  \u094D\u0937 \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F\u094B\u0902\
  \ \u0915\u093E \u0932\u093E\u092D \u0909\u0920\u093E\u0915\u0930 CSV \u092B\u093E\
  \u0907\u0932\u094B\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\
  \u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902 \u091C\u094B \u092A\u0922\u093C\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0914\u0930 `csv-writer` \u091C\u094B\
  \ \u0932\u093F\u0916\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F CSV \u092B\u093E\
  \u0907\u0932\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F \u0939\u0948\u0964\n\n\
  #."
title: "CSV \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 37
---

## कैसे:
TypeScript में, आप मूल कोड के माध्यम से या `csv-parser` जैसे तृतीय-पक्ष पुस्तकालयों का लाभ उठाकर CSV फाइलों के साथ काम कर सकते हैं जो पढ़ने के लिए और `csv-writer` जो लिखने के लिए CSV फाइलों के लिए है।

### `csv-parser` के साथ CSV पढ़ना
सबसे पहले, npm के माध्यम से `csv-parser` स्थापित करें:

```
npm install csv-parser
```

फिर, इस तरह से एक CSV फाइल पढ़ें:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // आउटपुट: ऑब्जेक्ट्स का एक ऐरे, प्रत्येक CSV में एक पंक्ति का प्रतिनिधित्व करता है
  });
```

मान लें `data.csv` में शामिल है:

```
name,age
Alice,30
Bob,25
```

आउटपुट होगा:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### `csv-writer` के साथ CSV लिखना
एक CSV फाइल में लिखने के लिए, पहले `csv-writer` स्थापित करें:

```
npm install csv-writer
```

फिर, इसे इस तरह उपयोग करें:

```typescript
import { createObjectCsvWriter as createCsvWriter } from 'csv-writer';

const csvWriter = createCsvWriter({
  path: 'out.csv',
  header: [
    {id: 'name', title: 'NAME'},
    {id: 'age', title: 'AGE'}
  ]
});

const data = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('The CSV file was written successfully'));
```

यह कोड `out.csv` में निम्नलिखित लिखता है:

```
NAME,AGE
Alice,30
Bob,25
```

ये उदाहरण दिखाते हैं कि आपके TypeScript परियोजनाओं में कुशलतापूर्वक CSV प्रक्रिया को कैसे एकीकृत कर सकते हैं, चाहे वह विश्लेषण के लिए डेटा पढ़ना हो या बाहरी रूप से एप्लिकेशन डेटा को संरक्षित करना हो।
