---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:21.294874-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Google Apps Script-\u098F, \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AA\u09CB\
  \u09B2\u09C7\u09B6\u09A8 template literals \u098F\u09B0 \u09AE\u09BE\u09A7\u09CD\
  \u09AF\u09AE\u09C7 \u0985\u09B0\u09CD\u099C\u09BF\u09A4 \u09B9\u09AF\u09BC\u0964\
  \ \u098F\u0997\u09C1\u09B2\u09BF \u098F\u0995\u09CD\u09B8\u09AA\u09CD\u09B0\u09C7\
  \u09B6\u09A8\u0997\u09C1\u09B2\u09BF \u098F\u09AE\u09CD\u09AC\u09C7\u09A1 \u0995\
  \u09B0\u09BE\u09B0 \u0985\u09A8\u09C1\u09AE\u09A4\u09BF \u09A6\u09C7\u09AF\u09BC\
  \ \u098F\u09AE\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u2026"
lastmod: '2024-03-17T18:47:43.507914-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script-\u098F, \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\
  \u09A8\u09CD\u099F\u09BE\u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 template literals\
  \ \u098F\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u0985\u09B0\u09CD\u099C\
  \u09BF\u09A4 \u09B9\u09AF\u09BC\u0964 \u098F\u0997\u09C1\u09B2\u09BF \u098F\u0995\
  \u09CD\u09B8\u09AA\u09CD\u09B0\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09BF \u098F\u09AE\
  \u09CD\u09AC\u09C7\u09A1 \u0995\u09B0\u09BE\u09B0 \u0985\u09A8\u09C1\u09AE\u09A4\
  \u09BF \u09A6\u09C7\u09AF\u09BC \u098F\u09AE\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09B2\u09BF\u099F\u09BE\u09B0\u09BE\u09B2, \u09AF\u09BE \u09B8\u09BE\
  \u09A7\u09BE\u09B0\u09A3 \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\u099A\u09BF\u09B9\
  \u09CD\u09A8\u09C7\u09B0 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09C7 \u09AC\
  \u09CD\u09AF\u09BE\u0995\u099F\u09BF\u0995\u09B8 (`) \u09A6\u09CD\u09AC\u09BE\u09B0\
  \u09BE \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u0986\u09AA\u09A8\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u09A4\u09BE\
  \u09A6\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কিভাবে:
Google Apps Script-এ, স্ট্রিং ইন্টারপোলেশন template literals এর মাধ্যমে অর্জিত হয়। এগুলি এক্সপ্রেশনগুলি এম্বেড করার অনুমতি দেয় এমন স্ট্রিং লিটারাল, যা সাধারণ উদ্ধৃতিচিহ্নের পরিবর্তে ব্যাকটিকস (`) দ্বারা চিহ্নিত। এখানে আপনি কিভাবে তাদের ব্যবহার করতে পারেন:

```javascript
// একটি বেসিক উদাহরণ
function basicInterpolationExample() {
  const user = 'Alice';
  console.log(`হ্যালো, ${user}!`); // আউটপুট: হ্যালো, Alice!
}

// এক্সপ্রেশন ব্যবহার করে
function expressionInterpolationExample() {
  const a = 5;
  const b = 10;
  console.log(`পাঁচ এবং দশ মিলে ${a + b}.`); // আউটপুট: পাঁচ এবং দশ মিলে 15.
}

// মাল্টি-লাইন স্ট্রিং
function multiLineStringExample() {
  const item = 'Google Apps Script';
  console.log(`এটা একটি মাল্টি-লাইন স্ট্রিং:
সবাইকে হ্যালো,
আজ আমরা ${item} নিয়ে আলোচনা করছি।`);
  // আউটপুট:
  // এটা একটি মাল্টি-লাইন স্ট্রিং:
  // সবাইকে হ্যালো,
  // আজ আমরা Google Apps Script নিয়ে আলোচনা করছি।
}

basicInterpolationExample();
expressionInterpolationExample();
multiLineStringExample();
```

এই উদাহরণগুলি বেসিক ব্যবহার, এক্সপ্রেশন এম্বেড করা, এবং মাল্টি-লাইন স্ট্রিংগুলি তৈরি করার সাথে ইন্টারপোলেটেড মানগুলি দেখায়।

## গভীরে ডুব
Template literals এবং স্ট্রিং ইন্টারপোলেশন, ECMAScript 2015 (ES6)-এ চালু হয়েছিল এবং পরবর্তীতে Google Apps Script-এ গৃহীত হয়েছিল। এর আগে, প্রোগ্রামারদের শুধুমাত্র স্ট্রিং কনক্যাটেনেশনের উপর নির্ভর করতে হত, যা জটিল স্ট্রিংগুলি বা অনেক পরিবর্তনশীল মান যুক্ত করার সময় অস্বস্তিকর হতে পারে।

```javascript
// পুরানো পদ্ধতি (ES6 এর আগে)
var user = 'Bob';
console.log('হ্যালো, ' + user + '!');
```

যদিও স্ট্রিং ইন্টারপোলেশন একটি শক্তিশালী বৈশিষ্ট্য, এর ব্যবহারের প্রসঙ্গগুলি নিয়ে সচেতন থাকা জরুরি। উদাহরণস্বরূপ, প্রোপার স্যানিটাইজেশন ছাড়া সরাসরি ইউজার ইনপুটের এমবেড করা ইনজেকশন আক্রমণের মতো নিরাপত্তা সমস্যার জন্ম দিতে পারে। Google Apps Script ডেভেলপারদের উচিত নিশ্চিত করা যে স্ট্রিংগুলিতে ইন্টারপোলেট করা যে কোনো ডাইনামিক কনটেন্ট যথাযথভাবে পরীক্ষা করা বা স্যানিটাইজ করা হয়েছে।

অন্যান্য প্রোগ্রামিং ভাষার তুলনায়, স্ট্রিং ইন্টারপোলেশনের ধারণা ব্যাপকভাবে বিদ্যমান, বিভিন্ন সিনট্যাক্সের সাথে। Python এ f-strings বা `format` পদ্ধতি ব্যবহার করে, Ruby ডাবল-উদ্ধৃতিচিহ্ন স্ট্রিংগুলির মধ্যে `#{}` ব্যবহার করে, এবং অনেক আধুনিক ভাষা পঠনযোগ্যতা এবং সুবিধার কারণে এই বৈশিষ্ট্যগুলি গ্রহণ করেছে।

Google Apps Script ECMAScript স্ট্যান্ডার্ডগুলি দ্বারা প্রদান করা ইন্টারপোলেশন বৈশিষ্ট্যগুলি ছাড়া অতিরিক্ত বৈশিষ্ট্য অফার করে না, তবে উপস্থিত কার্যকারিতা অধিকাংশ ব্যবহারের ক্ষেত্রে শক্তিশালী এবং যথেষ্ট। বিস্তৃত ইন্টারপোলেশন মেকানিজম সহ ভাষাগুলি থেকে আসা ডেভেলপারদের তাদের প্রত্যাশা সমন্বয় করতে হতে পারে, কিন্তু তারা সম্ভবত Google Apps Script-এ template literals এর সরলতা এবং দক্ষতাকে পছন্দ করবে।
