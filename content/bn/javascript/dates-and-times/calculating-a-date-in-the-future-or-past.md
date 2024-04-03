---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:39.155379-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7\u09B0 `Date` \u0985\u09AC\
  \u099C\u09C7\u0995\u09CD\u099F \u09B9\u09B2\u09CB \u0986\u09AA\u09A8\u09BE\u09B0\
  \ \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u099C\u09BF\u09AE\u09A8\u09CD\u09AF\
  \u09BE\u09B8\u09CD\u099F\u09BF\u0995\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AF\
  \u09BE\u0993\u09DF\u09BE\u09B0 \u09B8\u09CD\u09A5\u09BE\u09A8\u0964 \u099A\u09B2\
  \u09C1\u09A8 \u0995\u09BF\u099B\u09C1 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A8\
  \u09BF\u09AF\u09BC\u09C7 \u0996\u09C7\u09B2\u09BE \u0995\u09B0\u09BF."
lastmod: '2024-03-17T18:47:44.466667-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7\u09B0 `Date` \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F \u09B9\u09B2\
  \u09CB \u0986\u09AA\u09A8\u09BE\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0\
  \ \u099C\u09BF\u09AE\u09A8\u09CD\u09AF\u09BE\u09B8\u09CD\u099F\u09BF\u0995\u09C7\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09AF\u09BE\u0993\u09DF\u09BE\u09B0 \u09B8\u09CD\
  \u09A5\u09BE\u09A8\u0964 \u099A\u09B2\u09C1\u09A8 \u0995\u09BF\u099B\u09C1 \u0989\
  \u09A6\u09BE\u09B9\u09B0\u09A3 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0996\u09C7\u09B2\
  \u09BE \u0995\u09B0\u09BF."
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কিভাবে:
জাভাস্ক্রিপ্টের `Date` অবজেক্ট হলো আপনার তারিখের জিমন্যাস্টিকের জন্য যাওয়ার স্থান। চলুন কিছু উদাহরণ নিয়ে খেলা করি:

```javascript
// আজকের তারিখ
let today = new Date();
console.log(today); // বর্তমান তারিখ এবং সময় দেখায়

// ভবিষ্যতে 7 দিনের তারিখ গণনা
let nextWeek = new Date();
nextWeek.setDate(today.getDate() + 7);
console.log(nextWeek); // একই সময়ে, 7 দিন পরের তারিখ দেখায়

// অতীতে 30 দিনের তারিখ গণনা
let lastMonth = new Date();
lastMonth.setDate(today.getDate() - 30);
console.log(lastMonth); // একই সময়ে, 30 দিন আগের তারিখ দেখায়

// ভবিষ্যতে 1 বছরের তারিখ সেট করা
let nextYear = new Date();
nextYear.setFullYear(today.getFullYear() + 1);
console.log(nextYear); // একই সময়ে পরের বছরের তারিখ দেখায়
```
আউটপুটগুলি আপনি যখন এই কোডটি চালান তখন উপর নির্ভর করে, কারণ `today` আপনার বর্তমান তারিখ-সময়।

## গভীর ডুব দেওয়া
জাভাস্ক্রিপ্টে তারিখ-সংশোধনের জন্য অন্তর্ভুক্ত ফাংশন না থাকায়, প্রোগ্রামারদের মাসের দৈর্ঘ্যের পার্থক্য, লিপ ইয়ার এবং সময় অঞ্চলের পরিবর্তনের বিচার করে ম্যানুয়ালি তারিখ গণনা করতে হতো—এক বাস্তব ব্যথা! `Date` এর সাথে, এই ব্যথার অনেকটাই চলে যায়।

নেটিভ `Date` অবজেক্টের বিকল্প হিসাবে `moment.js` এবং `date-fns` এর মতো লাইব্রেরিগুলি রয়েছে, যা ঋতুন সিনট্যাক্স সরবরাহ করে এবং ডেলাইট সেভিং টাইমের মতো অদ্ভুত সমস্যাগুলি সমাধান করে।

তারিখ গণনা করার সময় মনে রাখবেন: `Date` মাসগুলি গণনা করে 0 (জানুয়ারি) থেকে 11 (ডিসেম্বর) পর্যন্ত, 1-12 নয়। এবং ফেব্রুয়ারির তারিখে কাজ করার সময় লিপ বছর ভুলে যাবেন না।

## আরও দেখুন
- MDN ওয়েব ডকস এ তারিখ: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- moment.js: https://momentjs.com/
- date-fns: https://date-fns.org/
