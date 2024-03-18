---
title:                "সংখ্যা নির্ণয়"
date:                  2024-03-17T18:15:15.745856-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
সংখ্যাকে গোল করা মানে হলো একটি সংখ্যাকে নির্দিষ্ট নির্ভুলতার মাত্রায় কেটে ফেলা। প্রোগ্রামাররা এটি করে থাকেন যাতে তারা সংখ্যাগত আউটপুটকে পঠনীয়তা, প্রদর্শনের উদ্দেশ্যে নিয়ন্ত্রণ করতে পারে অথবা যখন ভাসমান-বিন্দু ফলাফলের পরে নির্দিষ্ট নির্ভুলতা দরকার হয়।

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
