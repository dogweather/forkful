---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:17.140821-06:00
description: "TypeScript-\u098F \u09B0\u200D\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09AE\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\
  \u09B0 \u0985\u09B0\u09CD\u09A5 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09AA\u09B0\u09BF\u09B8\u09C0\u09AE\u09BE\u09B0 \u09AE\u09A7\u09CD\u09AF\
  \u09C7 \u0985\u09A8\u09BF\u09B0\u09CD\u09AD\u09AC\u09CD\u09AF \u09B8\u09BE\u0982\
  \u0996\u09CD\u09AF\u09BF\u0995 \u09AE\u09BE\u09A8 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u0985\u09A8\u09A8\u09CD\u09AF \u09B6\u09A8\u09BE\u0995\u09CD\u09A4\
  \u0995\u09BE\u09B0\u09C0 \u09A4\u09C8\u09B0\u09BF, \u09AA\u09B0\u09C0\u0995\u09CD\
  \u09B7\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\u2026"
lastmod: '2024-03-17T18:47:43.760613-06:00'
model: gpt-4-0125-preview
summary: "TypeScript-\u098F \u09B0\u200D\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09AE\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\
  \u09B0 \u0985\u09B0\u09CD\u09A5 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09AA\u09B0\u09BF\u09B8\u09C0\u09AE\u09BE\u09B0 \u09AE\u09A7\u09CD\u09AF\
  \u09C7 \u0985\u09A8\u09BF\u09B0\u09CD\u09AD\u09AC\u09CD\u09AF \u09B8\u09BE\u0982\
  \u0996\u09CD\u09AF\u09BF\u0995 \u09AE\u09BE\u09A8 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u0985\u09A8\u09A8\u09CD\u09AF \u09B6\u09A8\u09BE\u0995\u09CD\u09A4\
  \u0995\u09BE\u09B0\u09C0 \u09A4\u09C8\u09B0\u09BF, \u09AA\u09B0\u09C0\u0995\u09CD\
  \u09B7\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\u2026"
title: "\u098F\u09B2\u09CB\u09AE\u09C7\u09B2\u09CB \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u0989\u09CE\u09AA\u09A8\u09CD\u09A8 \u0995\u09B0\u09BE"
weight: 12
---

## কি এবং কেন?

TypeScript-এ র‍্যান্ডম সংখ্যা তৈরি করার অর্থ নির্দিষ্ট পরিসীমার মধ্যে অনির্ভব্য সাংখ্যিক মান তৈরি করা। প্রোগ্রামাররা অনন্য শনাক্তকারী তৈরি, পরীক্ষার জন্য ডেটা অনুকরণ অথবা গেম ও সিমুলেশনে অনির্দিষ্টতা যোগ করার মতো বিবিধ উদ্দেশ্যে এই র‍্যান্ডম সংখ্যা ব্যবহার করে থাকেন।

## কিভাবে:

TypeScript-এ, আপনি বিশ্বব্যাপী `Math` অবজেক্ট ব্যবহার করে র‍্যান্ডম সংখ্যা তৈরি করতে পারেন। নিচে বিভিন্ন দাবিদের জন্য র‍্যান্ডম সংখ্যা উৎপন্ন করার কিছু প্রাক্টিক্যাল উদাহরণ দেওয়া হলো।

### একটি বেসিক র‍্যান্ডম সংখ্যা তৈরি

০ (অন্তর্ভুক্ত) এবং ১ (বাদ দেওয়া) মধ্যে একটি বেসিক র‍্যান্ডম দশমিক সংখ্যা তৈরি করতে, আপনি `Math.random()` ব্যবহার করবেন। এতে কোন অতিরিক্ত ম্যানিপুলেশনের দরকার হয় না:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

এটি এমন একটি মান আউটপুট দিতে পারে যেমন `0.8995452185604771`.

### দুইটি নির্দিষ্ট মানের মধ্যে একটি র‍্যান্ডম পূর্ণসংখ্যা তৈরি

দুইটি নির্দিষ্ট মানের মধ্যে একটি পূর্ণসংখ্যা প্রয়োজন হলে, আপনাকে `Math.random()`-এর সাথে কিছু অংক সংক্রান্ত কার্য যুক্ত করতে হয়:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

এটি ১ এবং ১০-এর মধ্যে একটি পূর্ণসংখ্যা মান আউটপুট দিতে পারে, যেমন `7`.

### একটি অনন্য শনাক্তকারী তৈরি

র‍্যান্ডম সংখ্যা অন্যান্য পদ্ধতির সাথে মিলিত করে অনন্য শনাক্তকারী তৈরি করা যায়, যেমন, একটি সহজ UUID জেনারেটর স্নিপেট:

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

এটি একটি UUID-এর মতো স্ট্রিং উৎপন্ন করে, যেমন `110e8400-e29b-41d4-a716-446655440000`.

## গভীর ডাইভ

JavaScript এবং তদুপরি TypeScript-এ র‍্যান্ডম সংখ্যা উৎপন্নের প্রধান পদ্ধতি, `Math.random()`, একটি পিউডো-র‍্যান্ডম সংখ্যা জেনারেটর (PRNG)-এ নির্ভর করে। এটি লক্ষণীয় যে ফলাফল যদিও র‍্যান্ডম মনে হয়, তবে তা একটি আরম্ভিক বীজ মানের উপর ভিত্তি করে একটি নির্ধারিত অ্যালগরিদম দ্বারা উৎপন্ন হয়। তাই, `Math.random()` দ্বারা উৎপন্ন সংখ্যা প্রকৃতপক্ষে র‍্যান্ডম নয় এবং ক্রিপ্টোগ্রাফিক উদ্দেশ্যে ব্যবহারের জন্য উপযুক্ত নয়।

ক্রিপ্টোগ্রাফিকভাবে নিরাপদ র‍্যান্ডম সংখ্যার জন্য, Web Crypto API `crypto.getRandomValues()` প্রদান করে, যা Web Crypto মানদণ্ড সমর্থন করে এমন পরিবেশে অ্যাক্সেসযোগ্য, যার মধ্যে আধুনিক ব্রাউজার এবং Node.js (এর `crypto` মডিউলের মাধ্যমে) অন্তর্ভুক্ত। এখানে একটি দ্রুত উদাহরণ দেওয়া হলো যা TypeScript-এ একটি নিরাপদ র‍্যান্ডম সংখ্যা উৎপন্ন করার জন্য তার ব্যবহার দেখায়:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

এই পদ্ধতিটি উচ্চতর স্তরের র‍্যান্ডমতা প্রদান করে এবং নিরাপত্তা-সংবেদনশীল অ্যাপ্লিকেশনের জন্য অধিক উপযুক্ত। যাইহোক, এটি আরও সম্পদ-গহন এবং সাধারণ সিমুলেশন অথবা অকৃত্রিম র‍্যান্ডম মান উৎপন্নের মতো কম গুরুত্বপূর্ণ কাজের জন্য প্রয়োজনীয় নাও হতে পারে।
