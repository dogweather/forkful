---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:37.952383-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0995\u099F\u09BF JSON\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u099C\u09BE\u09AD\u09BE\
  \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u0985\u09AC\u099C\u09C7\u0995\
  \u09CD\u099F\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09A4\u09C7, `JSON.parse()`\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8\u0964."
lastmod: '2024-03-17T18:47:44.474736-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF JSON \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \u0995\u09C7 \u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u09C7 \u09AA\u09B0\u09BF\u09A3\
  \u09A4 \u0995\u09B0\u09A4\u09C7, `JSON.parse()` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C1\u09A8\u0964."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কিভাবে:


### JSON পার্সিং
একটি JSON স্ট্রিংকে জাভাস্ক্রিপ্ট অবজেক্টে পরিণত করতে, `JSON.parse()` ব্যবহার করুন।

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // আউটপুট: John
```

### জাভাস্ক্রিপ্ট অবজেক্টকে JSON স্ট্রিং-এ পরিণত করা
একটি জাভাস্ক্রিপ্ট অবজেক্টকে পুনরায় JSON স্ট্রিং-এ পরিণত করতে, `JSON.stringify()` ব্যবহার করুন।

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // আউটপুট: {"name":"Jane","age":25,"city":"London"}
```

### Node.js-এ ফাইলের সাথে কাজ করা
Node.js পরিবেশে একটি JSON ফাইল পড়ে তাকে একটি অবজেক্টে রূপান্তরিত করতে, আপনি `fs` মডিউল ব্যবহার করতে পারেন। এই উদাহরণে ধরা হয়েছে আপনার কাছে `data.json` নামে একটি ফাইল আছে।

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) throw err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

একটি অবজেক্টকে JSON ফাইলে লেখার জন্য:

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) throw err;
    console.log('ডেটা ফাইলে লেখা হয়েছে');
});
```

### থার্ড-পার্টি লাইব্রেরিস
জটিল JSON অপারেশনের জন্য, `lodash` এর মতো ফ্রেমওয়ার্ক এবং লাইব্রেরি কাজগুলি সহজ করে দিতে পারে, তবে মৌলিক অপারেশনের জন্য মূল জাভাস্ক্রিপ্ট ফাংশনগুলি প্রায়শই যথেষ্ট। বৃহত্তর আকার বা পারফরমেন্স-সংবেদনশীল অ্যাপ্লিকেশনের জন্য, আপনি দ্রুত JSON স্ট্রিংগিফাইয়ের জন্য `fast-json-stringify` বা আরও লচকযুক্ত JSON ফরম্যাট ব্যবহারের জন্য পার্স এবং স্ট্রিংগিফাই করার জন্য `json5` এর মতো লাইব্রেরিগুলি বিবেচনা করতে পারেন।

`json5` ব্যবহারে পার্সিং:
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // আউটপুট: John
```

এই উদাহরণগুলি জাভাস্ক্রিপ্টে JSON নিয়ে মৌলিক অপারেশন কভার করে, যা অন্যান্য ভাষা থেকে প্রবর্তনকারীদের জন্য আদর্শ এবং ওয়েব অ্যাপ্লিকেশনগুলিতে ডেটা দক্ষভাবে হ্যান্ডেল করার চেষ্টাকে তাক লাগানো।
