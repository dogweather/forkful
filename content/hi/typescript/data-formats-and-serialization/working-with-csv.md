---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:22.383905-07:00
description: "CSV (Comma-Separated Values) \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\
  \u092E \u0915\u0930\u0928\u093E \u0936\u093E\u092E\u093F\u0932 \u0939\u0948 CSV\
  \ \u092B\u093E\u0907\u0932\u094B\u0902 \u0938\u0947 \u092A\u0922\u093C\u0928\u093E\
  \ \u0914\u0930 \u0909\u0928\u092E\u0947\u0902 \u0932\u093F\u0916\u0928\u093E, \u091C\
  \u094B \u0915\u093F \u0907\u0938\u0915\u0940 \u0938\u093E\u0926\u0917\u0940 \u0914\
  \u0930 \u0935\u093F\u092D\u093F\u0928\u094D\u0928 \u092A\u094D\u0932\u0947\u091F\
  \u092B\u0949\u0930\u094D\u092E\u094B\u0902 \u0914\u0930 \u092D\u093E\u0937\u093E\
  \u0913\u0902 \u092E\u0947\u0902 \u0935\u094D\u092F\u093E\u092A\u0915\u2026"
lastmod: '2024-02-25T18:49:49.129567-07:00'
model: gpt-4-0125-preview
summary: "CSV (Comma-Separated Values) \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\
  \u092E \u0915\u0930\u0928\u093E \u0936\u093E\u092E\u093F\u0932 \u0939\u0948 CSV\
  \ \u092B\u093E\u0907\u0932\u094B\u0902 \u0938\u0947 \u092A\u0922\u093C\u0928\u093E\
  \ \u0914\u0930 \u0909\u0928\u092E\u0947\u0902 \u0932\u093F\u0916\u0928\u093E, \u091C\
  \u094B \u0915\u093F \u0907\u0938\u0915\u0940 \u0938\u093E\u0926\u0917\u0940 \u0914\
  \u0930 \u0935\u093F\u092D\u093F\u0928\u094D\u0928 \u092A\u094D\u0932\u0947\u091F\
  \u092B\u0949\u0930\u094D\u092E\u094B\u0902 \u0914\u0930 \u092D\u093E\u0937\u093E\
  \u0913\u0902 \u092E\u0947\u0902 \u0935\u094D\u092F\u093E\u092A\u0915\u2026"
title: "CSV \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

CSV (Comma-Separated Values) के साथ काम करना शामिल है CSV फाइलों से पढ़ना और उनमें लिखना, जो कि इसकी सादगी और विभिन्न प्लेटफॉर्मों और भाषाओं में व्यापक समर्थन के कारण प्रयोग होने वाला एक सामान्य डेटा आदान-प्रदान प्रारूप है। प्रोग्रामर्स एप्लिकेशनों, डेटाबेसों, और सेवाओं से डेटा आयात करने या निर्यात करने के लिए CSV फाइलों के साथ संलग्न होते हैं, जिससे डेटा को आसानी से छेड़छाड़ कर सकने और साझा करने की सुविधा मिलती है।

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
