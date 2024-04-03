---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:46.280488-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: Google Apps Script \u098F, \u09B2\
  \u0997\u09BF\u0982 \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09AA\u09A6\u09CD\
  \u09A7\u09A4\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u0995\u09B0\u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7, \u09AF\u09C7\
  \u09AE\u09A8 `Logger` \u0995\u09CD\u09B2\u09BE\u09B8 \u098F\u09AC\u0982 `console.log()`\u0964\
  \ Logger \u0995\u09CD\u09B2\u09BE\u09B8 \u09B9\u09B2\u09CB \u0990\u09A4\u09BF\u09B9\
  \u09CD\u09AF\u09AC\u09BE\u09B9\u09C0 \u0989\u09AA\u09BE\u09AF\u09BC,\u2026"
lastmod: '2024-03-17T18:47:43.532196-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u098F, \u09B2\u0997\u09BF\u0982 \u09AC\u09BF\u09AD\u09BF\
  \u09A8\u09CD\u09A8 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0995\u09B0\u09BE \u09AF\u09C7\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7, \u09AF\u09C7\u09AE\u09A8 `Logger` \u0995\u09CD\u09B2\
  \u09BE\u09B8 \u098F\u09AC\u0982 `console.log()`\u0964 Logger \u0995\u09CD\u09B2\u09BE\
  \u09B8 \u09B9\u09B2\u09CB \u0990\u09A4\u09BF\u09B9\u09CD\u09AF\u09AC\u09BE\u09B9\
  \u09C0 \u0989\u09AA\u09BE\u09AF\u09BC, \u09AF\u09BE \u09B8\u09BE\u09A7\u09BE\u09B0\
  \u09A3 \u09A1\u09BF\u09AC\u09BE\u0997\u09BF\u0982 \u098F\u09AC\u0982 \u09AC\u09BF\
  \u0995\u09BE\u09B6\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0989\u09AA\u09AF\u09CB\
  \u0997\u09C0\u0964 \u09B8\u09BE\u09AE\u09CD\u09AA\u09CD\u09B0\u09A4\u09BF\u0995\
  \ \u0986\u09AA\u09A1\u09C7\u099F\u0997\u09C1\u09B2\u09BF\u09B0 \u09B9\u09BF\u09B8\
  \u09C7\u09AC\u09C7, `console.log()` \u0986\u09B0\u0993 \u0985\u09A7\u09BF\u0995\
  \ \u09A8\u09AE\u09A8\u09C0\u09AF\u09BC\u09A4\u09BE \u098F\u09AC\u0982 Stackdriver\
  \ Logging \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u098F\u0995\u09C0\u09AD\u09C2\u09A4\
  \u0995\u09B0\u09A3 \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\u09C7, \u09AF\
  \u09BE Google Cloud Platform \u098F \u0986\u09AA\u09A8\u09BE\u09B0 Apps Scripts\
  \ \u09AE\u09A8\u09BF\u099F\u09B0 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u098F\u0995\u099F\u09BF \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0\
  \ \u09B8\u09AE\u09BE\u09A7\u09BE\u09A8 \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9 \u0995\
  \u09B0\u09C7\u0964\n\n**Logger \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE:**."
title: "\u09B2\u0997\u09BF\u0982"
weight: 17
---

## কীভাবে:
Google Apps Script এ, লগিং বিভিন্ন পদ্ধতি ব্যবহার করে করা যেতে পারে, যেমন `Logger` ক্লাস এবং `console.log()`। Logger ক্লাস হলো ঐতিহ্যবাহী উপায়, যা সাধারণ ডিবাগিং এবং বিকাশের জন্য উপযোগী। সাম্প্রতিক আপডেটগুলির হিসেবে, `console.log()` আরও অধিক নমনীয়তা এবং Stackdriver Logging এর সাথে একীভূতকরণ সরবরাহ করে, যা Google Cloud Platform এ আপনার Apps Scripts মনিটর করার জন্য একটি শক্তিশালী সমাধান সরবরাহ করে।

**Logger ব্যবহার করা:**

```javascript
function logSample() {
  Logger.log('This is a simple log message');
  
  var value = 5;
  Logger.log('The value is: %s', value); // স্ট্রিং ফর্ম্যাটিং
}

// লগ দেখতে:
// 1. logSample ফাংশনটি চালান।
// 2. দৃশ্য -> লগ
```

**Logger স্যাম্পল আউটপুট:**

```
[22-04-20 10:00:00:000 PDT] This is a simple log message
[22-04-20 10:00:00:001 PDT] The value is: 5
```

**console.log() ব্যবহার করা:**

```javascript
function consoleLogSample() {
  console.log('This message goes to Stackdriver Logging');
  const obj = {name: 'Jane', role: 'Developer'};
  console.info('Logging an object:', obj);
}

// Google Cloud Platform (GCP) কনসোলে Stackdriver Logging এর অধীনে লগ দেখা যাবে
```

**console.log() স্যাম্পল আউটপুট:**

```
This message goes to Stackdriver Logging
Logging an object: {name: "Jane", role: "Developer"}
```

জটিল অ্যাপ্লিকেশনগুলির জন্য `console.log()` এ স্থানান্তর করলে, বিকাশকারীরা GCP দ্বারা প্রদত্ত শক্তিশালী ফিল্টার এবং টুলগুলি ব্যবহার করে দক্ষতার সাথে লগ পার্স এবং বিশ্লেষণ করতে পারে, যা ঐতিহ্যবাহী Logger ক্লাসের সাথে ততটা সোজা নয়।

## প্রগাঢ় ডুব:
Google Apps Script এ লগিং যথেষ্ট উন্নত হয়েছে। প্রাথমিকভাবে, `Logger` ক্লাস ছিল বিকাশকারীদের তাদের স্ক্রিপ্টগুলি ডিবাগ করার প্রধান পদ্ধতি। এটি সহজ এবং মৌলিক স্ক্রিপ্টগুলির জন্য যথেষ্ট, কিন্তু আধুনিক ক্লাউড অ্যাপ্লিকেশনের জন্য প্রয়োজনীয় ক্ষমতা অভাব, যেমন লগ অনুসন্ধান করা বা সময়ের সাথে লগের প্রবণতা বিশ্লেষণ করা।

`console.log()` এর চালুকরণ এই ফাঁকটি পূরণ করেছে গুগল ক্লাউডের Stackdriver Logging (এখন অপারেশনস স্যুট নামে পরিচিত) এর সাথে Google Apps Script লগিংকে একীভূত করে, যা লগিং, মনিটরিং, এবং অ্যাপ্লিকেশনগুলি ডিবাগ করার জন্য একটি কেন্দ্রীয় প্ল্যাটফর্ম প্রদান করে। এটি শুধুমাত্র মাস্কাল লগিং অনুমতি দেয় না বরং লগ-ভিত্তিক মেট্রিক্স, রিয়েল-টাইম লগ বিশ্লেষণ, এবং অন্যান্য গুগল ক্লাউড সেবাগুলির সাথে একীভূতকরণের মতো উন্নত লগ ম্যানেজমেন্ট বৈশিষ্ট্য খোলা দেয়।

যদিও `Logger` এখনও ছোট স্ক্রিপ্টগুলিতে দ্রুত ডিবাগিং এবং লগিং এর জন্য একটি উদ্দেশ্য পরিবেশন করে, `console.log()` ব্যবহারের দিকে উন্নতি আধুনিক, ক্লাউড-নেটিভ অ্যাপ্লিকেশন ডেভেলপ করার প্রশস্ত পরিবর্তনের প্রতিফলন করে। এটি আজকের অ্যাপ্লিকেশনগুলির জটিলতা এবং স্কেল সম্বোধন করার জন্য ডেভেলপারদের সাথে সরঞ্জাম সরবরাহ করার গুগলের প্রতিশ্রুতিকে জোরদার করে। যাইহোক, নতুনদের জন্য Google Cloud Platform ধারণাগুলির সাথে পরিচিত হওয়ার আরও খানিকটা কঠিন শিখন প্রক্রিয়ার বিষয়ে সচেতন হওয়া উচিত। সত্ত্বেও, ক্লাউড ক্ষমতা পূর্ণমাত্রায় ব্যবহারের জন্য এই পদক্ষেপ ডেভেলপারদের জন্য সুবিধাজনক। ক্লাউড সেবাগুলির সাথে এই মানানসই করণ ক্লাউড কম্পিউটিং যুগে সুস্থির, স্কেলেবল লগিং প্রণালীর গুরুত্বকে জোরদার করে দেয়।
