---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:25.532299-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: TypeScript-\u098F, \u0986\u09AA\
  \u09A8\u09BF CSV \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09A8\
  \u09C7\u099F\u09BF\u09AD \u0995\u09CB\u09A1 \u0985\u09A5\u09AC\u09BE \u09A4\u09C3\
  \u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AF\u09C7\u09AE\u09A8 `csv-parser` \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09AA\u09A1\u09BC\u09BE\
  \ \u098F\u09AC\u0982 `csv-writer` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7 \u09B2\u09BF\u0996\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.788237-06:00'
model: gpt-4-0125-preview
summary: "TypeScript-\u098F, \u0986\u09AA\u09A8\u09BF CSV \u09AB\u09BE\u0987\u09B2\
  \u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09A8\u09C7\u099F\u09BF\u09AD \u0995\u09CB\
  \u09A1 \u0985\u09A5\u09AC\u09BE \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\
  \u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \ \u09AF\u09C7\u09AE\u09A8 `csv-parser` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 \u09AA\u09A1\u09BC\u09BE \u098F\u09AC\u0982 `csv-writer` \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B2\u09BF\u0996\u09BE\
  \ \u09AF\u09BE\u09AF\u09BC\u0964\n\n#."
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কিভাবে:
TypeScript-এ, আপনি CSV ফাইলের সাথে নেটিভ কোড অথবা তৃতীয় পক্ষের লাইব্রেরি যেমন `csv-parser` ব্যবহার করে পড়া এবং `csv-writer` ব্যবহার করে লিখা যায়।

### CSV পড়া `csv-parser` দ্বারা
প্রথমে, `csv-parser` ইনস্টল করুন npm এর মাধ্যমে:

```
npm install csv-parser
```

তারপর, একটি CSV ফাইল এভাবে পড়ুন:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // আউটপুট: অবজেক্টের একটি অ্যারে, প্রত্যেকটি CSV এর একটি সারিকে দর্শায়
  });
```

যদি `data.csv` এ থাকে:

```
name,age
Alice,30
Bob,25
```

তাহলে আউটপুট হবে:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### `csv-writer` দ্বারা CSV লিখা
CSV ফাইলে লিখতে, প্রথমে `csv-writer` ইনস্টল করুন:

```
npm install csv-writer
```

তারপর, এটি নিম্নলিখিত ভাবে ব্যবহার করুন:

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

এই কোড `out.csv` তে নিম্নলিখিত লিখবে:

```
NAME,AGE
Alice,30
Bob,25
```

এই উদাহরণগুলি দেখায় কিভাবে আপনার TypeScript প্রকল্পে কার্যকরভাবে CSV প্রক্রিয়াকরণ সমন্বিত করা যায়, তা সে ডাটা বিশ্লেষণের জন্য পড়া হোক বা অ্যাপ্লিকেশন ডাটা বাহ্যিকভাবে ধারণ করা হোক।
