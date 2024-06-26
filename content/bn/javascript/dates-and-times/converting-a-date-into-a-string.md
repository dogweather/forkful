---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:39.970663-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\
  \u0996\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  -\u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09AA\u09A6\
  \u09CD\u09A7\u09A4\u09BF\u0997\u09C1\u09B2\u09BF \u09B0\u09AF\u09BC\u09C7\u099B\u09C7\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC."
lastmod: '2024-03-17T18:47:44.464717-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996\u0997\u09C1\u09B2\u09BF\u0995\u09C7\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982-\u098F \u09B0\u09C2\u09AA\u09BE\u09A8\
  \u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\
  \u09B2\u09CD\u099F-\u0987\u09A8 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u0997\u09C1\
  \u09B2\u09BF \u09B0\u09AF\u09BC\u09C7\u099B\u09C7\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \u09C7\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC."
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 28
---

## কীভাবে:
জাভাস্ক্রিপ্টের তারিখগুলিকে স্ট্রিং-এ রূপান্তর করার জন্য বিল্ট-ইন পদ্ধতিগুলি রয়েছে। এখানে তাদের ব্যবহারের উপায়:

```javascript
const now = new Date();

// toLocaleString() - স্থানীয় ফর্ম্যাট
console.log(now.toLocaleString()); // '৪/১/২০২৩, ১২:০০:০০ PM' 

// toString() - মানক ফর্ম্যাট
console.log(now.toString()); // 'Sat Apr 01 2023 12:00:00 GMT+0100 (Central European Standard Time)'

// toISOString() - ISO ফর্ম্যাট (ডাটাবেস/নেটওয়ার্কের জন্য দুর্দান্ত)
console.log(now.toISOString()); // '২০২৩-০৪-০১T১১:০০:০০.০০০Z'
```

## গভীর ডুব
অতীতে, তারিখ থেকে স্ট্রিং রূপান্তর একটি গোলমেলে প্রক্রিয়া ছিল—কোন মান নেই, শুধু বিভিন্ন কাস্টম ফাংশন। ধন্যবাদস্বরূপ, ECMAScript প্রবেশ করে, ES5-এ তারিখ অবজেক্টকে মানকৃত করে এবং ES5.1-এ `toISOString()` যুক্ত করে যা অত্যন্ত সহায়ক।

নেটিভ পদ্ধতিগুলির বিকল্প হিসেবে `moment.js` এবং `date-fns` মতো লাইব্রেরিগুলি রয়েছে, যা আরো নিয়ন্ত্রণ এবং টাইম জোন হ্যান্ডলিং অফার করে, কিন্তু তারা আপনার প্রজেক্টের আকার বৃদ্ধি করে।

অভ্যন্তরীণভাবে, যখন আপনি একটি তারিখ-থেকে-স্ট্রিং পদ্ধতি কল করেন, জাভাস্ক্রিপ্ট সিস্টেমের লোকেল সেটিংস এবং টাইমজোন তথ্যের সাথে ইন্টার‌্যাক্ট করে স্ট্রিং আউটপুট উত্‌পন্ন করে। বিপরীতে, `toISOString()` সর্বদা একটি UTC সময় প্রদান করে (যেখানে 'Z' 'জুলু টাইম' বা UTC থেকে শূন্য অফসেটের জন্য দাঁড়ায়)।

## আরো দেখুন
- [MDN Web Docs – তারিখ](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [ISO 8601 তারিখ এবং সময় ফরম্যাট](https://www.iso.org/iso-8601-date-and-time-format.html)
- [date-fns](https://date-fns.org/)
