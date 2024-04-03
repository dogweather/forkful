---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:17:44.253188-06:00
description: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B9\u09B2 \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\
  \u09BE\u09B0\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AF\u09CB\u0997\u09BE\u09AF\
  \u09CB\u0997\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u0989\u09AA\u09BE\u09AF\u09BC\
  , \u09AF\u09BE \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u0985\u09A8\u09C1\
  \u09B8\u09BE\u09B0\u09C7 \u09A1\u09C7\u099F\u09BE \u0986\u09A8\u09BE \u09AC\u09BE\
  \ \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB \u0995\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u09AA\u09BF\u0986\
  \u0987 \u0985\u09A5\u09AC\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\
  \u09CD\u09AD\u09BF\u09B8\u0997\u09C1\u09B2\u09BF\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.494415-06:00'
model: gpt-4-0125-preview
summary: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B9\u09B2 \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\
  \u09BE\u09B0\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AF\u09CB\u0997\u09BE\u09AF\
  \u09CB\u0997\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u0989\u09AA\u09BE\u09AF\u09BC\
  , \u09AF\u09BE \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u0985\u09A8\u09C1\
  \u09B8\u09BE\u09B0\u09C7 \u09A1\u09C7\u099F\u09BE \u0986\u09A8\u09BE \u09AC\u09BE\
  \ \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB \u0995\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u09AA\u09BF\u0986\
  \u0987 \u0985\u09A5\u09AC\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\
  \u09CD\u09AD\u09BF\u09B8\u0997\u09C1\u09B2\u09BF\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u09AF\u09CB\u0997\u09BE\u09AF\u09CB\u0997\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7, \u09AF\u09BE\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\
  \u09C7 \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u0997\
  \u09C1\u09B2\u09BF \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09A8\u09C7\u099F\u09C7\
  \ \u09B0\u09BF\u09B8\u09CB\u09B0\u09CD\u09B8, \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\
  \u09B8 \u098F\u09AC\u0982 \u09A1\u09C7\u099F\u09BE \u0985\u09CD\u09AF\u09BE\u0995\
  \u09CD\u09B8\u09C7\u09B8 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964\
  ."
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

## কিভাবে:
Fish এ HTTP অনুরোধ পাঠানোর জন্য বিল্ট-ইন কমান্ড নেই, তবে আপনি সরাসরি শেল থেকে `curl` ব্যবহার করতে পারেন:

```Fish
curl http://api.example.com/data
```

জেসন ডেটা সহ একটি POST অনুরোধের জন্য:

```Fish
curl -X POST -H "Content-Type: application/json" -d '{"key":"value"}' http://api.example.com/data
```

উত্তরটি সংরক্ষণ করতে:

```Fish
set response (curl -X GET http://api.example.com/data)
```

এবং এখানে আপনি GET অনুরোধের পরে যা দেখতে পারেন:

```Fish
{
  "response": "সার্ভার থেকে কিছু ডেটা"
}
```

## গভীরে যাওয়া
ইতিহাসের দিক থেকে, UNIX এবং Linux শেলগুলি নেটওয়ার্কিং কাজের জন্য সুবিধাজনক। প্রাথমিক দিনগুলিতে, এই উদ্দেশ্যে `telnet` মত টুলগুলি প্রচলিত ছিল। আজকের দিনে, `curl` এবং `wget` এর মত ইউটিলিটি প্রোগ্রাম প্রধান হয়ে উঠেছে। `curl` একটি বহুমুখী টুল যা একাধিক প্রোটোকল সমর্থন করে, এবং এর সারল্য ও নমনীয়তার কারণে প্রায়ই ব্যবহার করা হয়।

আরও জটিল অনুরোধ হাতলের জন্য Python অথবা Node.js ব্যবহার করা যেতে পারে। তবে দ্রুত কাজের জন্য অথবা সাধারণ স্ক্রিপ্টের জন্য, Fish এ `curl` কার্যকর এবং কার্যক্ষম।

Fish এর মাধ্যমে HTTP অনুরোধ বাস্তবায়ন সাধারণত মানে তৃতীয় পক্ষের টুলগুলির উপর নির্ভরশীলতা। Fish নিজেই একটি স্মার্ট এবং ব্যবহারকারী-বান্ধব কমান্ড-লাইন শেল হিসেবে ডিজাইন করা হয়েছে, সব-কিছু করার একটি টুল নয়। যখন আপনি এটি `curl` এর মতো ইউটিলিটিগুলির শক্তির সাথে সংমিশ্রণ করেন, আপনি উভয়ের সেরা পান: Fish এর ব্যবহারযোগ্যতা এবং `curl` এর ক্ষমতা।

## আরও দেখুন
- `curl` সম্পর্কে আরও জানুন: https://curl.se/docs/manual.html
- Fish Shell ডকুমেন্টেশন: https://fishshell.com/docs/current/index.html
- HTTP বেসিকস ওভারভিউ: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview
- `curl` এর একটি বিকল্প হিসেবে `httpie` সঙ্গে এপিআই অন্বেষণ করুন: https://httpie.io/
