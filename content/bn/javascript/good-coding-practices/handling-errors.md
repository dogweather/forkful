---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:00.302748-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u0995\u09CD\u09B2\u09BE\u09B8\u09BF\u0995 `try-catch` \u09AC\u09CD\u09B2\u0995\
  \u09C7\u09B0 \u098F\u0995\u099F\u09BF \u0989\u09A6\u09BE\u09B9\u09B0\u09A3."
lastmod: '2024-03-17T18:47:44.460777-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09CD\u09B2\u09BE\u09B8\u09BF\u0995\
  \ `try-catch` \u09AC\u09CD\u09B2\u0995\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u0989\
  \u09A6\u09BE\u09B9\u09B0\u09A3."
title: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u0997\u09C1\u09B2\u09BF \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE"
weight: 16
---

## কিভাবে:
এখানে ক্লাসিক `try-catch` ব্লকের একটি উদাহরণ:

```javascript
try {
  // কোড যেটা এরর ছুড়তে পারে
  let result = potentiallyRiskyOperation();
  console.log('Success:', result);
} catch (error) {
  // যদি কোন এরর ছুড়ে তাহলে কি করবে
  console.error('Oops:', error.message);
}
```

যখন কোন এরর ঘটে না, তখন স্যাম্পল আউটপুট:
```
Success: 42
```

এবং যখন কোন এরর হয়:
```
Oops: কিছু ভুল হয়েছে
```

অ্যাসিঙ্ক্রোনাস কোডের জন্য, যেখানে প্রমিস জড়িত আছে, একটি `async` ফাংশনে `try-catch` ব্যবহার করুন:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Data fetched:', data);
  } catch (error) {
    console.error('Error fetching data:', error.message);
  }
}

fetchData();
```

## গভীর ডাইভ
জাভাস্ক্রিপ্টে এরর হ্যান্ডলিং ইভলভ করেছে। পুরোনো দিনে (ES3, সার্কা 1999), আমাদের শুধুমাত্র `try-catch` ব্লক ছিল। অত্যন্ত নমনীয় না হলেও, এটি কাজ করেছিল।

ES6 (2015) প্রমিস এবং `.then()` এবং `.catch()` উপস্থাপন করে, আমাদের অ্যাসিঙ্ক্রোনাস এরর সমূহকে আরও সুন্দর ভাবে হ্যান্ডল করার সুযোগ দিয়েছে।

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Data fetched:', data))
  .catch(error => console.error('Error fetching data:', error.message));
```

বাস্তবায়ন বিবরণের কথা বলতে, যখন কোন এরর ছোঁড়া হয়, জাভাস্ক্রিপ্ট ইঞ্জিনগুলি `Error` অবজেক্ট সৃষ্টি করে যা `message` এবং `stack` এর মতো উপকারী প্রোপার্টিসমূহ ধারণ করে। আপনি `Error` ক্লাস এক্সটেন্ড করে কাস্টম এরর টাইপও তৈরি করতে পারেন - জটিল অ্যাপসের জন্য হাতের কাছে।

বিকল্প? আপনি এরর হ্যান্ডলিং এড়িয়ে যেতে পারেন (খারাপ ধারণা), এরর-প্রথম প্যারামিটার সহ কলব্যাক ব্যবহার করতে পারেন (হ্যালো, Node.js স্টাইল), অথবা আরও আধুনিক লাইব্রেরি এবং ফ্রেমওয়ার্কের সাথে আরো আটটিভ কিছু দেখতে পারেন।

## আরও দেখুন
এরর হ্যান্ডলিং সম্পর্কে আরো জানুন:

- MDN এ try-catch সম্পর্কে: [MDN try...catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN async function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- প্রমিসের গাইড: [MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- কাস্টম এরর তৈরি এবং ছুঁড়ে দেওয়া: [MDN Error](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error)
