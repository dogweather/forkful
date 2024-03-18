---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:40.191488-06:00
description: "\u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE \u098F\u0995\u099F\u09BF \u09AE\u09CC\
  \u09B2\u09BF\u0995 \u0995\u09BE\u099C, \u09AF\u09BE \u0986\u099C\u0995\u09C7\u09B0\
  \ \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC \u09AA\
  \u09C1\u09A8\u09B0\u09C1\u09A6\u09CD\u09A7\u09BE\u09B0 \u098F\u09AC\u0982 \u09B8\
  \u09AE\u09CD\u09AD\u09AC\u09A4 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8\
  \ \u0995\u09B0\u09BE \u099C\u09A1\u09BC\u09BF\u09A4\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0993\
  \u09AF\u09BC\u09C7\u09AC\u09B8\u09BE\u0987\u099F\u0997\u09C1\u09B2\u09BF\u09A4\u09C7\
  \u2026"
lastmod: '2024-03-17T18:47:44.463733-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE \u098F\u0995\u099F\u09BF \u09AE\u09CC\
  \u09B2\u09BF\u0995 \u0995\u09BE\u099C, \u09AF\u09BE \u0986\u099C\u0995\u09C7\u09B0\
  \ \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC \u09AA\
  \u09C1\u09A8\u09B0\u09C1\u09A6\u09CD\u09A7\u09BE\u09B0 \u098F\u09AC\u0982 \u09B8\
  \u09AE\u09CD\u09AD\u09AC\u09A4 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8\
  \ \u0995\u09B0\u09BE \u099C\u09A1\u09BC\u09BF\u09A4\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0993\
  \u09AF\u09BC\u09C7\u09AC\u09B8\u09BE\u0987\u099F\u0997\u09C1\u09B2\u09BF\u09A4\u09C7\
  \u2026"
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
---

{{< edit_this_page >}}

## কি এবং কেন?
জাভাস্ক্রিপ্টে বর্তমান তারিখ পাওয়া একটি মৌলিক কাজ, যা আজকের তারিখ এবং সময় পুনরুদ্ধার এবং সম্ভবত পরিবর্তন করা জড়িত। প্রোগ্রামাররা এটি ওয়েবসাইটগুলিতে তারিখ প্রদর্শন, অ্যাপ্লিকেশনে, ব্যবহারকারীর ইন্টারঅ্যাকশন ট্র্যাক করা, অথবা সময়-সংবেদনশীল ডেটা হ্যান্ডেল করার জন্য সম্পাদন করে।

## কিভাবে:
ভ্যানিলা জাভাস্ক্রিপ্টে, `Date` অবজেক্টটি তারিখ এবং সময় নিয়ে কাজ করার জন্য ব্যবহৃত হয়। এখানে আপনি কিভাবে বর্তমান তারিখ এবং সময় পেতে পারেন:

```javascript
const currentDate = new Date();
console.log(currentDate); // উদাহরণ আউটপুট: Fri Apr 14 2023 12:34:56 GMT+0100 (British Summer Time)
```

আরও ব্যবহারকারী-বান্ধব ফর্ম্যাটে শুধুমাত্র তারিখটি প্রদর্শন করতে আপনি `toLocaleDateString()` মেথডের মত মেথড ব্যবহার করতে পারেন:

```javascript
console.log(currentDate.toLocaleDateString()); // উদাহরণ আউটপুট: 4/14/2023
```

ফর্ম্যাট নিয়ন্ত্রণে আরও সমৃদ্ধ বিকল্পের জন্য, *Moment.js* অথবা *date-fns* মত তৃতীয়-পক্ষের লাইব্রেরীগুলি খুব জনপ্রিয়, যদিও মনে রাখা ভালো যে Moment.js এখন রক্ষণাবেক্ষণ মোডে একটি লেগ্যাসি প্রকল্প হিসেবে বিবেচিত হয়।

*Moment.js* ব্যবহার করে:

```javascript
const moment = require('moment'); // Node.js বা একটি মডিউল বান্ডলার ব্যবহার করে অনুমান করা হয়েছে
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // উদাহরণ আউটপুট: 2023-04-14
```

*date-fns* ব্যবহার করে, যা মডিউলারাইজেশনে জোর দেয় যাতে আপনি কেবল আপনার প্রয়োজন মত ইম্পোর্ট করতে পারেন:

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // উদাহরণ আউটপুট: 2023-04-14
```

প্রতিটি পদ্ধতি জাভাস্ক্রিপ্টে তারিখ নিয়ে কাজ করার জন্য ভিন্ন মাত্রার সুবিধা এবং নমনীয়তা প্রদান করে, অভ্যন্তরীণ `Date` অবজেক্ট থেকে লাইব্রেরীগুলি মাধ্যমে পাওয়া আরও উন্নত ফর্ম্যাটিং এবং পরিবর্তনের সুযোগ পর্যন্ত।
