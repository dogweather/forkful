---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:15:15.745856-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: TypeScript \u098F \u09B8\u0982\
  \u0996\u09CD\u09AF\u09BE \u0997\u09CB\u09B2 \u0995\u09B0\u09BE \u09AC\u09C7\u09B6\
  \ \u0995\u09BF\u099B\u09C1 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u09B0 \u09AE\u09BE\
  \u09A7\u09CD\u09AF\u09AE\u09C7 \u0995\u09B0\u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\u09CD\u09B0\u09C1\
  \u09A4 \u09AA\u09CD\u09B0\u09B8\u09CD\u09A4\u09C1\u09A4\u09BF \u09A6\u09C7\u0993\
  \u09DF\u09BE \u09B9\u09DF\u09C7\u099B\u09C7."
lastmod: '2024-03-17T18:47:43.759626-06:00'
model: gpt-4-0125-preview
summary: "TypeScript \u098F \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0997\u09CB\u09B2\
  \ \u0995\u09B0\u09BE \u09AC\u09C7\u09B6 \u0995\u09BF\u099B\u09C1 \u09AA\u09A6\u09CD\
  \u09A7\u09A4\u09BF\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u0995\u09B0\
  \u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7 \u09A6\u09CD\u09B0\u09C1\u09A4 \u09AA\u09CD\u09B0\u09B8\u09CD\u09A4\
  \u09C1\u09A4\u09BF \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\u09DF\u09C7\u099B\u09C7\
  ."
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
weight: 13
---

## কিভাবে:
TypeScript এ সংখ্যা গোল করা বেশ কিছু পদ্ধতির মাধ্যমে করা যেতে পারে। এখানে দ্রুত প্রস্তুতি দেওয়া হয়েছে:

```typescript
// Math.round হলো নিকটতম পূর্ণসংখ্যায় গোল করা
console.log(Math.round(1.5)); // আউটপুট: 2

// Math.ceil হলো উপরের দিকে নিকটতম পূর্ণসংখ্যায় গোল করা
console.log(Math.ceil(1.1)); // আউটপুট: 2

// Math.floor হলো নিচের দিকে নিকটতম পূর্ণসংখ্যায় গোল করা
console.log(Math.floor(1.8)); // আউটপুট: 1

// toFixed হলো নির্ধারিত দশমিক স্থানে গোল করা
let num = 1.23456;
console.log(num.toFixed(2)); // আউটপুট: "1.23"
// নোট: toFixed একটি স্ট্রিং ফেরত দেয়! প্রয়োজনে parseFloat ব্যবহার করে পুনরায় রূপান্তর করুন।
console.log(parseFloat(num.toFixed(2))); // আউটপুট: 1.23
```

## গভীরে ডুব:
অতীতে, সীমিত স্থান এবং নির্ভুলতা সমস্যার কারণে সংখ্যাকে গোল করা একটি অপরিহার্য কাজ ছিল। আজকের দিনে, ভাসমান-বিন্দু অ্যারিথমেটিক বাইনারি মধ্যে সংখ্যাগুলি কিভাবে সঞ্চিত হয় তার কারণে কিছু অদ্ভুত ফলাফল আনতে পারে। গোল করার বিকল্পগুলি অন্তর্ভুক্ত হলো ফ্লোর, সিল, এবং ট্রাঙ্ক (দশমিক ছাড়াই কাটা কিন্তু গোল নয়)।

অন্তঃপ্রাণ ব্যাপারগুলি মনে রাখা উচিত: `Math.round` "গোল হাফ আপ" (অথবা "বাণিজ্যিক গোল") অনুসরণ করে, যখন `Math.floor` এবং `Math.ceil` সরল। `toFixed` অনাকাঙ্ক্ষিত ফলাফল আনতে পারে কারণ এটি একটি স্ট্রিং ফেরত 
দেয়, এবং এটি "রাউন্ড হাফ টু ইভেন" (অথবা "ব্যাংকার্স রাউন্ডিং") ব্যবহার করে গোল করে, বারবার একই সংখ্যাকে গোল করার ক্ষেত্রে পক্ষপাত হ্রাস করতে বিশেষ কার্যকর।

## আরও দেখুন
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE ভাসমান-বিন্দু অ্যারিথমেটিকের জন্য স্ট্যান্ডার্ড (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
