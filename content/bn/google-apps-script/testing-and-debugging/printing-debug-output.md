---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:07:52.742731-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Google Apps Script \u09AE\u09CC\
  \u09B2\u09BF\u0995 \u09A1\u09BF\u09AC\u09BE\u0997\u09BF\u0982 \u098F\u09B0 \u099C\
  \u09A8\u09CD\u09AF `Logger` \u0995\u09CD\u09B2\u09BE\u09B8 \u098F\u09AC\u0982 \u0986\
  \u09B0\u0993 \u0989\u09A8\u09CD\u09A8\u09A4 \u099A\u09BE\u09B9\u09BF\u09A6\u09BE\
  \ \u09B8\u09AE\u09CD\u09AA\u09C2\u09B0\u09CD\u09A3 \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF V8 \u09B0\u09BE\u09A8\u099F\u09BE\u0987\u09AE\u09C7 \u099A\u09BE\
  \u09B2\u09C1 \u0995\u09B0\u09BE `console` \u0995\u09CD\u09B2\u09BE\u09B8 \u09AA\u09CD\
  \u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964\u2026"
lastmod: '2024-04-05T21:53:51.510151-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u09AE\u09CC\u09B2\u09BF\u0995 \u09A1\u09BF\u09AC\u09BE\
  \u0997\u09BF\u0982 \u098F\u09B0 \u099C\u09A8\u09CD\u09AF `Logger` \u0995\u09CD\u09B2\
  \u09BE\u09B8 \u098F\u09AC\u0982 \u0986\u09B0\u0993 \u0989\u09A8\u09CD\u09A8\u09A4\
  \ \u099A\u09BE\u09B9\u09BF\u09A6\u09BE \u09B8\u09AE\u09CD\u09AA\u09C2\u09B0\u09CD\
  \u09A3 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF V8 \u09B0\u09BE\u09A8\u099F\
  \u09BE\u0987\u09AE\u09C7 \u099A\u09BE\u09B2\u09C1 \u0995\u09B0\u09BE `console` \u0995\
  \u09CD\u09B2\u09BE\u09B8 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\
  \u0964 **Logger \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7:**\
  \ Logger \u0995\u09CD\u09B2\u09BE\u09B8 \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09A1\
  \u09BF\u09AC\u09BE\u0997 \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE \u09B2\u0997 \u0995\
  \u09B0\u09A4\u09C7 \u0985\u09A8\u09C1\u09AE\u09A4\u09BF \u09A6\u09C7\u09AF\u09BC\
  , \u09AF\u09BE \u0986\u09AA\u09A8\u09BF \u09A8\u09BF\u09B0\u09CD\u09AC\u09BE\u09B9\
  \u09A8\u09C7\u09B0 \u09AA\u09B0\u09C7 Apps Script \u09B8\u09AE\u09CD\u09AA\u09BE\
  \u09A6\u0995\u09C7 `View > Logs` \u098F\u09B0 \u0985\u09A7\u09C0\u09A8\u09C7 \u09A6\
  \u09C7\u0996\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u0989\u09A6\u09BE\u09B9\
  \u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কিভাবে:
Google Apps Script মৌলিক ডিবাগিং এর জন্য `Logger` ক্লাস এবং আরও উন্নত চাহিদা সম্পূর্ণ করার জন্য V8 রানটাইমে চালু করা `console` ক্লাস প্রদান করে।

**Logger ব্যবহার করে:**

Logger ক্লাস আপনাকে ডিবাগ বার্তা লগ করতে অনুমতি দেয়, যা আপনি নির্বাহনের পরে Apps Script সম্পাদকে `View > Logs` এর অধীনে দেখতে পারেন। এখানে একটি সহজ উদাহরণ দেওয়া হল:

```javascript
function logSample() {
  var name = "Wired Reader";
  Logger.log("Hello, %s!", name);
}
```

`logSample()` চালানোর পরে, আপনি লগ ভিউয়ারে "Hello, Wired Reader!" সাথে লগটি দেখতে পারেন।

**V8 রানটাইমের সাথে console.log ব্যবহার করা:**

V8 রানটাইমের সাথে, `console.log` অন্যান্য ভাষাগুলি থেকে ডেভেলপারদের জন্য একটি আরও পরিচিত সিনট্যাক্স প্রদান করে:

```javascript
function consoleSample() {
  var status = 'active';
  var count = 150;
  console.log(`Current status: ${status}, Count: ${count}`);
}
```

নির্বাহনের পরে, আউটপুট দেখার জন্য `View > Stackdriver Logging` -এ Stackdriver Logging অ্যাক্সেস করুন। এটি আরও শক্তিশালী, স্ট্রিং ইন্টারপোলেশন এবং অবজেক্ট পরীক্ষার সমর্থন করে, এবং Google Cloud-এর লগিং এর সাথে সমন্বয় করে, স্থায়ী লগ এবং উন্নত ফিল্টারিং ক্ষমতা সরবরাহ করে।

**console.log থেকে নমুনা আউটপুট:**

```
Current status: active, Count: 150
```

## গভীর পর্যালোচনা
প্রাথমিকভাবে, `Logger.log` Google Apps Script-এ ডিবাগিং এর প্রধান সরঞ্জাম ছিল, পরিদর্শনের জন্য আউটপুট প্রিন্ট করার একটি সহজ, সরল উপায় প্রদান করে। যাইহোক, স্ক্রিপ্টগুলি আরও জটিল এবং Google Cloud Platform সেবাগুলির সাথে সম্পৃক্ত হওয়ায়, আরও একটি সবল লগিং সমাধানের প্রয়োজন প্রকট হয়।

V8 রানটাইমের অবতারণা, `console.log`-কে অনুসরণীয় করে। এটি Google Apps Script-কে শুধু স্ট্যান্ডার্ড JavaScript সিনট্যাক্সের সাথে সমমূল্যে নিয়ে আসে না, বরং ডেভেলপারদেরকে যারা JavaScript এর সাথে পরিচিত Google Cloud-এর শক্তিশালী অবকাঠামোর সুবিধা গ্রহণ করার সুযোগ দেয়। `console.log`-এর প্রবর্তন এবং Google Cloud Platform এর সাথে এর সমন্বয় Google Apps Script এর মধ্যে ডিবাগিং ক্ষমতাগুলির উল্লেখযোগ্য বিবর্তনের চিহ্ন দেয়, ডেভেলপারদেরকে তাদের স্ক্রিপ্টগুলিকে মনিটর করা এবং সমস্যা নির্ণয় করতে আরও গতিশীল ও স্কেলযোগ্য পদ্ধতি প্রদান করে।

যদিও `Logger.log` মৌলিক ডিবাগিং চাহিদা এবং ছোট প্রকল্পের জন্য যথেষ্ট, V8 রানটাইমের সাথে `console.log` আরও বিস্তৃত এবং ভবিষ্যৎ-প্রমাণিত সমাধান অফার করে। এতে নির্বাহন সেশনের বাইরে লগগুলি রাখার সক্ষমতা, Google Cloud কনসোলের মধ্যে লগগুলি অনুসন্ধান এবং ফিল্টার করার সুযোগ, এবং আধুনিক JavaScript ডেভেলপমেন্ট প্র্যাক্টিসের সাথে সামঞ্জস্য অন্তর্ভুক্ত। যাইহোক, ডেভেলপারদের উচিত এই বিকল্পগুলির মধ্যে চয়ন করার সময় তাদের প্রকল্পের জটিলতা এবং আকার অনুযায়ী তাদের চাহিদা বিচার করা।
