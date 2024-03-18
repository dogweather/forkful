---
title:                "JSON এর সাথে কাজ করা"
date:                  2024-03-17T18:29:37.952383-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

JSON (জাভাস্ক্রিপ্ট অবজেক্ট নোটেশন) একটি হালকা ডেটা-ইন্টারচেঞ্জ ফরম্যাট, যা মানুষের পড়া এবং লেখা এবং মেশিনের পার্স এবং জেনারেট করা সহজ। প্রোগ্রামাররা ওয়েব অ্যাপ্লিকেশনগুলিতে ডেটা সংরক্ষণ এবং পরিবহনের জন্য এটি ব্যবহার করে, যা আধুনিক API এবং ওয়েব সেবা যোগাযোগের মেরুদণ্ড করে তোলে।

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
