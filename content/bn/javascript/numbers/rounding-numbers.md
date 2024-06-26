---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:14:47.544431-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2\u09CB \u099C\u09BE\u09AD\u09BE\
  \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7 `Math.round()`, `Math.ceil()`,\
  \ \u098F\u09AC\u0982 `Math.floor()` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\u09B2\u09BF \u0995\
  \u09C0\u09AD\u09BE\u09AC\u09C7 \u09B0\u09BE\u0989\u09A8\u09CD\u09A1 \u0995\u09B0\
  \u09A4\u09C7 \u09B9\u09AF\u09BC."
lastmod: '2024-03-17T18:47:44.447639-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\
  \u09B2\u09CB \u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7 `Math.round()`, `Math.ceil()`, \u098F\u09AC\u0982 `Math.floor()` \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u0982\u0996\u09CD\
  \u09AF\u09BE\u0997\u09C1\u09B2\u09BF \u0995\u09C0\u09AD\u09BE\u09AC\u09C7 \u09B0\
  \u09BE\u0989\u09A8\u09CD\u09A1 \u0995\u09B0\u09A4\u09C7 \u09B9\u09AF\u09BC."
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
weight: 13
---

## কিভাবে:
এখানে দেখানো হলো জাভাস্ক্রিপ্টে `Math.round()`, `Math.ceil()`, এবং `Math.floor()` ব্যবহার করে সংখ্যাগুলি কীভাবে রাউন্ড করতে হয়: 

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (যেহেতু .567 হলো .5 এর চেয়ে বেশি)

console.log(roundedDown); // প্রিন্ট করে: 2
console.log(roundedUp);   // প্রিন্ট করে: 3
console.log(rounded);     // প্রিন্ট করে: 3
```

নির্দিষ্ট দশমিকের স্থানের সংখ্যা ফিক্স করার জন্য, `toFixed()` ব্যবহার করুন:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (একটি স্ট্রিং ফিরে আসে)

console.log(twoDecimals); // প্রিন্ট করে: "2.57"
```

স্ট্রিংটিকে আবার একটি সংখ্যায় পরিণত করতে একটি ইউনারি প্লাস বা `Number()` ব্যবহার করুন:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // প্রিন্ট করে: 2.57
```

## গভীরে ডুব দেওয়া
সংখ্যাগুলি রাউন্ড করা নতুন কিছু নয়; এটি সংখ্যার পুরানো ধারণা। জাভাস্ক্রিপ্টে, `Math.round()` "round half up" টাই-ব্রেকিং ব্যবহার করে: যদি ভগ্নাংশ অংশ 0.5 হয়, তাহলে এটি নিকটতম সমান সংখ্যায় রাউন্ড করে।

আরও নিয়ন্ত্রণের জন্য, `toFixed()` হতে পারে আপনার গন্তব্য, তবে মনে রাখবেন, এটি একটি স্ট্রিং ফেরত দেয়। সংখ্যা হিসাবে আবার পরিণত করা একটি অতিরিক্ত ধাপ হলেও এটি নিশ্চিত করে যে আপনি সাংখ্যিক টাইপের সাথে কাজ করে চলেছেন। 

বিকল্প? `lodash` এর মতো লাইব্রেরিগুলি `_.round(number, [precision=0])` অফার করে আরও নিঁখুত নিয়ন্ত্রণের জন্য। অথবা, নতুন `Intl.NumberFormat` আপনাকে শুধুমাত্র রাউন্ডিং নয়, উচ্চ-নির্ভুলতা ফর্ম্যাটিং প্রদান করে।

যথাযথতার কথা বলতে গেলে, জাভাস্ক্রিপ্টে ফ্লোটিং-পয়েন্টের অদ্ভুত বিষয়গুলির সম্পর্কে সাবধান থাকুন। `0.1 + 0.2` ঠিক `0.3` এর সমান নয়, কারণ সংখ্যাগুলি কীভাবে সঞ্চিত হয় তা নিয়ে। মাঝে মাঝে, এই রকম ফ্লোটিং-পয়েন্টের ভুলগুলি ঠিক করতে রাউন্ডিং প্রয়োজন হতে পারে।

## আরও দেখুন
- মজিলার গাণিতিক ডকুমেন্টেশন: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- `Intl.NumberFormat` এর মাধ্যমে আর্থিক রাউন্ডিং: [ECMAScript Internationalization API](https://tc39.es/ecma402/#numberformat-objects)
- `lodash` রাউন্ডিং: [Lodash Docs](https://lodash.com/docs/4.17.15#round)
