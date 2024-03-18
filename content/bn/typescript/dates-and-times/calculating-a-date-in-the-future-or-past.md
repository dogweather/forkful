---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:46.636437-06:00
description: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u0985\u09A5\u09AC\u09BE\
  \ \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\u0995\
  \u09C7 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09C7 \u09A6\
  \u09C7\u0996\u09BE \u09AF\u09C7, \u09AC\u09B2\u09C1\u09A8 \u098F\u0996\u09A8 \u09A5\
  \u09C7\u0995\u09C7 \u09E7\u09E6 \u09A6\u09BF\u09A8 \u09AA\u09B0\u09C7 \u0995\u09CB\
  \u09A8 \u09A6\u09BF\u09A8 \u09AA\u09A1\u09BC\u09AC\u09C7, \u0985\u09A5\u09AC\u09BE\
  \ \u09E7\u09E6 \u09A6\u09BF\u09A8 \u0986\u0997\u09C7 \u0995\u09CB\u09A8 \u09A6\u09BF\
  \u09A8 \u099B\u09BF\u09B2\u0964\u2026"
lastmod: '2024-03-17T18:47:43.779010-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u0985\u09A5\u09AC\u09BE \u0985\
  \u09A4\u09C0\u09A4\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09AC\
  \u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7\
  \ \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09C7 \u09A6\u09C7\
  \u0996\u09BE \u09AF\u09C7, \u09AC\u09B2\u09C1\u09A8 \u098F\u0996\u09A8 \u09A5\u09C7\
  \u0995\u09C7 \u09E7\u09E6 \u09A6\u09BF\u09A8 \u09AA\u09B0\u09C7 \u0995\u09CB\u09A8\
  \ \u09A6\u09BF\u09A8 \u09AA\u09A1\u09BC\u09AC\u09C7, \u0985\u09A5\u09AC\u09BE \u09E7\
  \u09E6 \u09A6\u09BF\u09A8 \u0986\u0997\u09C7 \u0995\u09CB\u09A8 \u09A6\u09BF\u09A8\
  \ \u099B\u09BF\u09B2\u0964\u2026"
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

ভবিষ্যত অথবা অতীতের একটি তারিখ গণনা করা মানে বর্তমান তারিখকে পরিবর্তন করে দেখা যে, বলুন এখন থেকে ১০ দিন পরে কোন দিন পড়বে, অথবা ১০ দিন আগে কোন দিন ছিল। প্রোগ্রামাররা এই কাজ করেন যেমন মেয়াদ উত্তীর্ণ তারিখ, ইভেন্ট নির্ধারণ, অথবা সময় পার্থক্য নির্ণয়ের জন্য।

## কিভাবে:

```TypeScript
// বর্তমান তারিখ পেতে
const today: Date = new Date();

// ভবিষ্যতের ১০ দিন গণনা করা
const tenDaysLater: Date = new Date(today.getTime() + (10 * 24 * 60 * 60 * 1000));
console.log(`এখন থেকে দশ দিন পরে: ${tenDaysLater.toDateString()}`);

// অতীতের ১০ দিন গণনা করা
const tenDaysBefore: Date = new Date(today.getTime() - (10 * 24 * 60 * 60 * 1000));
console.log(`দশ দিন আগে ছিল: ${tenDaysBefore.toDateString()}`);
```
নমুনা আউটপুট:
```
এখন থেকে দশ দিন পরে: রবি এপ্রিল ২৩ ২০২৩
দশ দিন আগে ছিল: বুধ এপ্রিল ০৩ ২০২৩
```

## গভীর ডুব

ঐতিহাসিক ভাবে, JavaScript—এবং তার এক্সটেনশন TypeScript—এ তারিখ সম্পর্কিত ব্যবস্থাপনা জটিল হয়ে ওঠে তারিখ অবজেক্ট এবং টাইমজোনের বিচিত্র দিকের কারণে। এর জটিলতা সরানোর জন্য Moment.js এবং date-fns এর মত বিকল্প লাইব্রেরি অ্যাবস্ট্র্যাকশনের সুবিধা দেয়। ES6 এর মধ্যে দিয়ে, `Intl` API এর মাধ্যমে আরও ভালো আন্তর্জাতিকীকরণের সমর্থন পেয়েছিল, যেখানে TypeScript এটি ব্যবহার করতে পারে।

তারিখ গণনা করার সময়, দিনের আলো সংরক্ষণের পরিবর্তন এবং লিপ সেকেন্ডের মতো বিষয়গুলোর জন্য সচেতন থাকুন। এগুলো একটি তারিখে ২৪ ঘন্টা যোগ করার মত সোজা গণণাগুলোকে বিপর্যয়ে ফেলতে পারে। এছাড়াও, গণনা করা তারিখ প্রদর্শনের সময় ব্যবহারকারীর লোকেল এবং টাইমজোনের কথা সবসময় মনে রাখুন।

প্রশস্ত সমর্থন এবং নমনীয়তার জন্য, আপনি `date-fns` অথবা `Luxon` এর মতো লাইব্রেরিগুলো বেছে নিতে পারেন, যা মডুলার এবং জটিল কাজের জন্য দারুণ। উদাহরণস্বরূপ, `date-fns` এর সাথে, আপনি সহজেই দিন যোগ করতে পারেন:

```TypeScript
import { addDays } from 'date-fns';

const result = addDays(new Date(2023, 3, 13), 10); // এপ্রিল ১৩, ২০২৩ + ১০ দিন
console.log(result.toDateString());
```

তারা প্রান্তিক কেসগুলো এবং টাইমজোন সংক্রান্ত সমস্যাগুলিও সামাল দেয়, তারিখের অঙ্কের ব্যথা অনেকাংশে লাঘব করে।

## আরও দেখুন

- [MDN তারিখ রেফারেন্স](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns লাইব্রেরি](https://date-fns.org/)
- [Luxon ডকুমেন্টেশন](https://moment.github.io/luxon/#/)
- [TypeScript অফিসিয়াল ডকুমেন্টেশন](https://www.typescriptlang.org/docs/)
