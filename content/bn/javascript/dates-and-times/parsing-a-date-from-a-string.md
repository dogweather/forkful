---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:06:08.827090-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: JavaScript \u09B8\u09CD\u09AC\u09BE\
  \u09AD\u09BE\u09AC\u09BF\u0995\u09AD\u09BE\u09AC\u09C7 `Date.parse()` \u09AE\u09C7\
  \u09A5\u09A1 \u098F\u09AC\u0982 `Date` \u0995\u09A8\u09CD\u09B8\u099F\u09CD\u09B0\
  \u09BE\u0995\u09CD\u099F\u09B0 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\
  \u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \u0997\u09C1\u09B2\u09BF \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3 \u0995\
  \u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\u0964 \u09A4\u09AC\u09C7, \u098F\u0987\
  \ \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u0997\u09C1\u09B2\u09CB\u09B0 \u09AC\u09BF\
  \u09AD\u09BF\u09A8\u09CD\u09A8\u2026"
lastmod: '2024-04-05T21:53:53.107770-06:00'
model: gpt-4-0125-preview
summary: "JavaScript \u09B8\u09CD\u09AC\u09BE\u09AD\u09BE\u09AC\u09BF\u0995\u09AD\u09BE\
  \u09AC\u09C7 `Date.parse()` \u09AE\u09C7\u09A5\u09A1 \u098F\u09AC\u0982 `Date` \u0995\
  \u09A8\u09CD\u09B8\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F\u09B0 \u09AA\u09CD\u09B0\
  \u09A6\u09BE\u09A8 \u0995\u09B0\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF \u09AC\u09BF\u09B6\u09CD\u09B2\
  \u09C7\u09B7\u09A3 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\u0964 \u09A4\
  \u09AC\u09C7, \u098F\u0987 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u0997\u09C1\u09B2\
  \u09CB\u09B0 \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09AC\u09CD\u09B0\u09BE\
  \u0989\u099C\u09BE\u09B0\u09C7 \u09B8\u09C0\u09AE\u09BE\u09AC\u09A6\u09CD\u09A7\u09A4\
  \u09BE \u098F\u09AC\u0982 \u0985\u09B8\u0982\u0997\u09A4\u09BF \u09B0\u09AF\u09BC\
  \u09C7\u099B\u09C7, \u09AC\u09BF\u09B6\u09C7\u09B7 \u0995\u09B0\u09C7 \u0985-\u09AE\
  \u09BE\u09A8\u0995 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AC\u09BF\u09A8\u09CD\u09AF\
  \u09BE\u09B8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\u0964 \u098F\u0987 \u09B8\u09AE\
  \u09B8\u09CD\u09AF\u09BE\u0997\u09C1\u09B2\u09BF \u09AE\u09CB\u0995\u09BE\u09AC\u09C7\
  \u09B2\u09BE \u0995\u09B0\u09A4\u09C7, `Moment.js` \u098F\u09AC\u0982 `date-fns`\
  \ \u09AE\u09A4\u09CB \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\u09B7\
  \u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09A4\u09BE\
  \u09A6\u09C7\u09B0 \u09B8\u09B9\u099C \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u098F\u09AC\u0982 \u09AC\u09BF\u09B6\u09CD\u09AC\u09BE\u09B8\u09AF\u09CB\u0997\
  \u09CD\u09AF\u09A4\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u099C\u09A8\u09AA\u09CD\
  \u09B0\u09BF\u09AF\u09BC\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কিভাবে:
JavaScript স্বাভাবিকভাবে `Date.parse()` মেথড এবং `Date` কন্সট্রাক্টর প্রদান করে তারিখ স্ট্রিংগুলি বিশ্লেষণ করার জন্য। তবে, এই পদ্ধতিগুলোর বিভিন্ন ব্রাউজারে সীমাবদ্ধতা এবং অসংগতি রয়েছে, বিশেষ করে অ-মানক তারিখ বিন্যাসের সাথে। এই সমস্যাগুলি মোকাবেলা করতে, `Moment.js` এবং `date-fns` মতো তৃতীয় পক্ষের লাইব্রেরি তাদের সহজ ব্যবহার এবং বিশ্বাসযোগ্যতার জন্য জনপ্রিয়।

### নেটিভ JavaScript ব্যবহার করে:
```javascript
const dateString = "2023-04-30T14:55:00";
const dateObj = new Date(dateString);

console.log(dateObj);  // আউটপুট: Sun Apr 30 2023 14:55:00 GMT+0000 (Coordinated Universal Time)
```

### Moment.js ব্যবহার করে:
প্রথমে, npm এর মাধ্যমে Moment.js ইন্সটল করুন অথবা আপনার প্রজেক্টে অন্তর্ভুক্ত করুন। তারপর:
```javascript
const moment = require('moment');

const dateString = "2023-04-30T14:55:00";
const dateObj = moment(dateString);

console.log(dateObj.toString());  // আউটপুট: Sun Apr 30 2023 14:55:00 GMT+0000
```

### date-fns ব্যবহার করে:
আপনার প্রজেক্টে `date-fns` যোগ করার পর, এরকম একটি তারিখ স্ট্রিং বিশ্লেষণ করুন:
```javascript
const { parseISO } = require('date-fns');

const dateString = "2023-04-30T14:55:00";
const dateObj = parseISO(dateString);

console.log(dateObj);  // আউটপুট: 2023-04-30T14:55:00.000Z
```

`Moment.js` এবং `date-fns` উভয়েই বিভিন্ন ফরম্যাট এবং লোকেল হ্যান্ডল করার সহিত আরও সম্পূর্ণ বিশ্লেষণ ক্ষমতা প্রদান করে, যা জটিল অ্যাপ্লিকেশনের জন্য তাদেরকে প্রাধান্যযোগ্য করে তোলে।
