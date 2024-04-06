---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:06:39.648394-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Google Apps Script, \u09AF\u09BE\
  \ JavaScript \u098F\u09B0 \u0989\u09AA\u09B0 \u09AD\u09BF\u09A4\u09CD\u09A4\u09BF\
  \ \u0995\u09B0\u09C7 \u09A4\u09C8\u09B0\u09BF, \u0986\u09AA\u09A8\u09BF \u098F\u0995\
  \u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7\
  \ \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09C7\u09B6 \u0995\u09AF\u09BC\u09C7\u0995\
  \u099F\u09BF \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u09A8\
  \u09BF\u099A\u09C7\u2026"
lastmod: '2024-04-05T21:53:51.527503-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, \u09AF\u09BE JavaScript \u098F\u09B0 \u0989\u09AA\u09B0\
  \ \u09AD\u09BF\u09A4\u09CD\u09A4\u09BF \u0995\u09B0\u09C7 \u09A4\u09C8\u09B0\u09BF\
  , \u0986\u09AA\u09A8\u09BF \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\
  \u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09C7\
  \u09B6 \u0995\u09AF\u09BC\u09C7\u0995\u099F\u09BF \u09AA\u09A6\u09CD\u09A7\u09A4\
  \u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u09A8\u0964 \u09A8\u09BF\u099A\u09C7 \u09AE\u09C2\u09B2 JavaScript\
  \ \u09AE\u09C7\u09A5\u09A1 \u098F\u09AC\u0982 Google Apps Script \u0987\u0989\u099F\
  \u09BF\u09B2\u09BF\u099F\u09BF\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\
  \u0993\u09AF\u09BC\u09BE \u09B9\u09DF\u09C7\u099B\u09C7\u0964 **`new Date()` \u0995\
  \u09A8\u09B8\u09CD\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F\u09B0 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7:** Google Apps Script-\u098F \u098F\u0995\
  \u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09A4\u09BE\
  \u09B0\u09BF\u0996\u09C7 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0\
  \ \u09B8\u09AC\u099A\u09C7\u09AF\u09BC\u09C7 \u09B8\u09B9\u099C \u0989\u09AA\u09BE\
  \u09AF\u09BC \u09B9\u09B2\u09CB `Date` \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\
  \u09C7\u09B0 \u0995\u09A8\u09B8\u09CD\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F\u09B0\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u0964 \u09A4\u09AC\
  \u09C7, \u098F\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982\u099F\u09BF\u0995\u09C7 Date.parse() \u09AE\u09C7\
  \u09A5\u09A1 \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u099A\u09BF\u09A8\u09A4\u09C7\
  \ \u09B8\u0995\u09CD\u09B7\u09AE \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\
  \u09C7 \u09A5\u09BE\u0995\u09A4\u09C7 \u09B9\u09AC\u09C7 (\u09AF\u09C7\u09AE\u09A8\
  , YYYY-MM-DD)\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কিভাবে:
Google Apps Script, যা JavaScript এর উপর ভিত্তি করে তৈরি, আপনি একটি স্ট্রিং থেকে তারিখ পার্স করার জন্য বেশ কয়েকটি পদ্ধতি ব্যবহার করতে পারেন। নিচে মূল JavaScript মেথড এবং Google Apps Script ইউটিলিটিগুলি ব্যবহার করে উদাহরণ দেওয়া হয়েছে।

**`new Date()` কনস্ট্রাক্টর ব্যবহার করে:**

Google Apps Script-এ একটি স্ট্রিংকে তারিখে পার্স করার সবচেয়ে সহজ উপায় হলো `Date` অবজেক্টের কনস্ট্রাক্টর ব্যবহার করা। তবে, এটি তারিখের স্ট্রিংটিকে Date.parse() মেথড দ্বারা চিনতে সক্ষম ফর্ম্যাটে থাকতে হবে (যেমন, YYYY-MM-DD)।

```javascript
const dateString = '2023-04-01';
const dateObject = new Date(dateString);
Logger.log(dateObject); // লগ করে Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)
```

**`Utilities.parseDate()` ব্যবহার করে:**

আরো বেশি লচকতা পেতে, বিশেষ করে কাস্টম তারিখ ফর্ম্যাটের ক্ষেত্রে, Google Apps Script `Utilities.parseDate()` প্রদান করে। এই পদ্ধতিটি আপনাকে তারিখের ফর্ম্যাট, টাইমজোন এবং লোকেল নির্দিষ্ট করতে দেয়

```javascript
const dateString = '01-04-2023'; // DD-MM-YYYY
const format = 'dd-MM-yyyy';
const timezone = Session.getScriptTimeZone();
const dateObject = Utilities.parseDate(dateString, timezone, format);
Logger.log(dateObject); // লগ করে Sat Apr 01 2023 00:00:00 GMT+0000 (UTC) স্ক্রিপ্টের টাইমজোনের উপর নির্ভর করে
```

নোট: `Utilities.parseDate()` আরো নিয়ন্ত্রণ প্রদান করে, তবে এর আচরণ স্ক্রিপ্টের টাইমজোনের উপর ভিত্তি করে ভিন্ন হতে পারে, তাই যদি আপনার অ্যাপ্লিকেশন একাধিক অঞ্চল জুড়ে তারিখ পরিচালনা করে তাহলে টাইমজোনটি স্পষ্টভাবে নির্দিষ্ট করা জরুরি।

## গভীর ডাইভ
তারিখ পার্সিং প্রোগ্রামিং ভাষায় ঐতিহাসিকভাবে চ্যালেঞ্জিং হিসেবে পরিচিত ছিল, মূলত তারিখের ফর্ম্যাটগুলির বৈচিত্র্য এবং সময় অঞ্চলগুলির জটিলতার কারণে। Google Apps Script-এর পদ্ধতি, মূলত JavaScript থেকে উদ্ভূত, সরল ভাবে এই জটিলতাগুলি সমাধান করার লক্ষ্য নিয়েছে, এটি সরল `Date` অবজেক্ট এবং অধিক বহুমুখী `Utilities.parseDate()` ফাংশন উভয়ই উপস্থাপন করে। তবে, প্রতিটি পদ্ধতির সীমাবদ্ধতা রয়েছে; উদাহরণস্বরূপ, স্ট্রিংগুলির সাথে `Date` কনস্ট্রাক্টরের উপর নির্ভর করা বিভিন্ন পরিবেশের বিভিন্ন রকমের তারিখ ফর্ম্যাটের ব্যাখ্যা এর কারণে অসঙ্গতি সৃষ্টি করে। অপরদিকে, `Utilities.parseDate()` ফর্ম্যাট, টাইমজোন, এবং লোকেলের পরিষ্কার বোঝার প্রয়োজন হয়, যা এটিকে কিছুটা জটিল কিন্তু নির্দিষ্ট প্রয়োজনীয়তার জন্য আরো বিশ্বস্ত করে তোলে।

Moment.js (এখন নতুন প্রকল্পগুলির জন্য Luxon প্রস্তাবনা করা) এর মতো বিকল্প লাইব্রেরি বা সার্ভিসগুলি, সমৃদ্ধ কার্যকারিতা এবং ভাল জোন হ্যান্ডলিং সরবরাহ করে, এই চ্যালেঞ্জগুলির অনেকগুলি মোকাবিলা করে। তবে, Google Apps Script-এর প্রসঙ্গে, যেখানে বহিরাগত লাইব্রেরিগুলির সীমাবদ্ধতা রয়েছে, অন্তর্নিহিত পদ্ধতিগুলিকে কার্যকরভাবে ব্যবহার করার বোঝা এবং লিভারেজ করা গুরুত্বপূর্ণ হয়ে ওঠে। অন্যান্য ভাষা থেকে আসা প্রোগ্রামাররা Google Apps Script-এ তারিখের হ্যান্ডলিংয়ের নানাবিধ বিবেচনাগুলি অনন্য চ্যালেঞ্জিং মনে করতে পারে তবে উপলব্ধ টুলস এবং তাদের অ্যাপ্লিকেশনের বৈশ্বিক প্রকৃতির সুনির্দিষ্ট বিবেচনা করে তারা শক্তিশালী তারিখ পার্সিং অর্জন করতে পারে।
