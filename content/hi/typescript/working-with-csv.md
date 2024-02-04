---
title:                "CSV के साथ काम करना"
date:                  2024-02-03T19:22:22.383905-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
