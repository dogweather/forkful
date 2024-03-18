---
title:                "CSV এর সাথে কাজ করা"
date:                  2024-03-17T18:28:25.532299-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

CSV (Comma-Separated Values) এর সাথে কাজ করা মানে হলো CSV ফাইল পড়া এবং লিখা, যা একটি সাধারণ ডাটা এক্সচেঞ্জ ফর্ম্যাট হিসেবে ব্যবহার করা হয় এর সাধারণতা এবং বিভিন্ন প্ল্যাটফর্ম ও ভাষায় ব্যাপক সমর্থনের কারণে। প্রোগ্রামারগণ অ্যাপ্লিকেশন, ডেটাবেস এবং সেবা থেকে ডাটা ইম্পোর্ট অথবা এক্সপোর্ট করতে CSV ফাইলের সাথে কাজ করে থাকেন, যা সহজে ডাটা ম্যানিপুলেশন এবং শেয়ারিং সম্ভব করে।

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
