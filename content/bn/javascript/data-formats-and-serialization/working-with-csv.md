---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:14.561400-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7\u09B0 \u09AC\u09BF\u09B2\u09CD\
  \u099F-\u0987\u09A8 CSV \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u09AC\u09BE\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09BF\u09AB\u09BE\u0987 \u0995\
  \u09B0\u09BE\u09B0 \u09AB\u09BE\u0982\u09B6\u09A8\u09BE\u09B2\u09BF\u099F\u09BF\
  \ \u09A8\u09C7\u0987 \u09AF\u09C7\u09AE\u09A8 \u099C\u09C7\u09B8\u09A8\u09C7\u09B0\
  \ \u0995\u09CD\u09B7\u09C7\u09A4\u09CD\u09B0\u09C7 \u0986\u099B\u09C7\u0964 \u09A4\
  \u09AC\u09C7, \u0986\u09AA\u09A8\u09BF \u09B8\u09B9\u099C \u099C\u09BE\u09AD\u09BE\
  \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09B9\u099C\u09C7\u0987\u2026"
lastmod: '2024-03-17T18:47:44.475737-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7\u09B0 \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 CSV \u09AA\u09BE\u09B0\
  \u09CD\u09B8\u09BF\u0982 \u09AC\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \u0997\u09BF\u09AB\u09BE\u0987 \u0995\u09B0\u09BE\u09B0 \u09AB\u09BE\u0982\u09B6\
  \u09A8\u09BE\u09B2\u09BF\u099F\u09BF \u09A8\u09C7\u0987 \u09AF\u09C7\u09AE\u09A8\
  \ \u099C\u09C7\u09B8\u09A8\u09C7\u09B0 \u0995\u09CD\u09B7\u09C7\u09A4\u09CD\u09B0\
  \u09C7 \u0986\u099B\u09C7\u0964 \u09A4\u09AC\u09C7, \u0986\u09AA\u09A8\u09BF \u09B8\
  \u09B9\u099C \u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09B9\
  \u099C\u09C7\u0987 CSV \u09A1\u09C7\u099F\u09BE \u09AE\u09CD\u09AF\u09BE\u09A8\u09C7\
  \u099C \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09AC\u09C7\u09A8 \u09AC\u09BE\
  \ \u0986\u09B0\u0993 \u099C\u099F\u09BF\u09B2 \u09B8\u09BF\u09A8\u09BE\u09B0\u09BF\
  \u0993\u09B0 \u099C\u09A8\u09CD\u09AF `PapaParse` \u098F\u09B0 \u09AE\u09A4\u09CB\
  \ \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09AC\u09C7\u09A8\u0964\n\n#."
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কিভাবে:
জাভাস্ক্রিপ্টের বিল্ট-ইন CSV পার্সিং বা স্ট্রিংগিফাই করার ফাংশনালিটি নেই যেমন জেসনের ক্ষেত্রে আছে। তবে, আপনি সহজ জাভাস্ক্রিপ্ট ব্যবহার করে সহজেই CSV ডেটা ম্যানেজ করতে পারবেন বা আরও জটিল সিনারিওর জন্য `PapaParse` এর মতো শক্তিশালী লাইব্রেরি ব্যবহার করতে পারবেন।

### র জাভাস্ক্রিপ্টের মাধ্যমে বেসিক পার্সিং
একটি সাধারণ CSV স্ট্রিংকে একটি অবজেক্টের অ্যারেতে পার্স করতে:

```javascript
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

function parseCSV(csv) {
  const lines = csv.split("\n");
  const result = [];
  const headers = lines[0].split(",");

  for (let i = 1; i < lines.length; i++) {
    const obj = {};
    const currentline = lines[i].split(",");
    
    for (let j = 0; j < headers.length; j++) {
      obj[headers[j]] = currentline[j];
    }
    result.push(obj);
  }
  
  return result;
}

console.log(parseCSV(csv));
```
আউটপুট:

```
[
  { name: 'John', age: '23', city: 'New York' },
  { name: 'Jane', age: '28', city: 'Los Angeles' }
]
```

### র জাভাস্ক্রিপ্টের মাধ্যমে বেসিক CSV জেনারেশন
অবজেক্টের একটি অ্যারেকে CSV স্ট্রিংএ রূপান্তর করতে:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

function arrayToCSV(arr) {
  const csv = arr.map(row => 
    Object.values(row).join(',')
  ).join('\n');
  
  return csv;
}

console.log(arrayToCSV(data));
```

আউটপুট:

```
John,23,New York
Jane,28,Los Angeles
```

### জটিল CSV কাজের জন্য PapaParse ব্যবহার করা
আরও জটিল সিনারিওর জন্য, `PapaParse` একটি দৃঢ় লাইব্রেরি যা প্রবাহ, ওয়ার্কার এবং বিশাল ফাইল সামলানোর অপশন সহ CSV ফাইল পার্সিং এবং স্ট্রিংগিফাই করতে যথেষ্ট।

PapaParse দিয়ে CSV ফাইল বা স্ট্রিং পার্স করা:

```javascript
// আপনার প্রজেক্টে PapaParse যোগ করার পর
const Papa = require('papaparse');
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

Papa.parse(csv, {
  complete: function(results) {
    console.log("Parsed:", results.data);
  }
});
```

জেনারেট করে:

```
Parsed: [
  ["name", "age", "city"],
  ["John", "23", "New York"],
  ["Jane", "28", "Los Angeles"]
]
```

PapaParse দিয়ে একটি অ্যারেকে CSV স্ট্রিংএ রূপান্তর করা:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

console.log(Papa.unparse(data));
```

জেনারেট করে:

```
name,age,city
John,23,New York
Jane,28,Los Angeles
```

এই উদাহরণগুলি জাভাস্ক্রিপ্টে বেসিক এবং অ্যাডভান্সড CSV হ্যান্ডলিং দেখায়, যা ওয়েব অ্যাপ্লিকেশন এবং তার বাইরে সহজে ডেটা বিনিময় করতে সাহায্য করে।
