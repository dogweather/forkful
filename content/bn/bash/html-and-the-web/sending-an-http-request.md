---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:17:06.301077-06:00
description: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B9\u09B2 \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\
  \u09BE\u09B0\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AF\u09CB\u0997\u09BE\u09AF\
  \u09CB\u0997 \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u0989\u09AA\u09BE\
  \u09AF\u09BC, \u09AF\u09BE \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u09A1\u09C7\u099F\
  \u09BE \u09AA\u09C1\u09A8\u09B0\u09C1\u09A6\u09CD\u09A7\u09BE\u09B0 \u0995\u09B0\
  \u09BE \u09AC\u09BE \u09AB\u09B0\u09CD\u09AE \u099C\u09AE\u09BE \u09A6\u09C7\u0993\
  \u09AF\u09BC\u09BE \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\
  \u09A8 \u0993\u09AF\u09BC\u09C7\u09AC\u2026"
lastmod: '2024-03-17T18:47:44.223010-06:00'
model: gpt-4-0125-preview
summary: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B9\u09B2 \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\
  \u09BE\u09B0\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AF\u09CB\u0997\u09BE\u09AF\
  \u09CB\u0997 \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u0989\u09AA\u09BE\
  \u09AF\u09BC, \u09AF\u09BE \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u09A1\u09C7\u099F\
  \u09BE \u09AA\u09C1\u09A8\u09B0\u09C1\u09A6\u09CD\u09A7\u09BE\u09B0 \u0995\u09B0\
  \u09BE \u09AC\u09BE \u09AB\u09B0\u09CD\u09AE \u099C\u09AE\u09BE \u09A6\u09C7\u0993\
  \u09AF\u09BC\u09BE \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\
  \u09A8 \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\u09B8\
  , API-\u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AF\u09CB\u0997\u09BE\u09AF\u09CB\
  \u0997 \u0995\u09B0\u09A4\u09C7 \u09AC\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\
  \u09BE\u09AE\u0997\u09CD\u09B0\u09C0 \u09B8\u0982\u0995\u09CD\u09B0\u09BE\u09A8\u09CD\
  \u09A4 \u0995\u09BE\u099C \u09B8\u09CD\u09AC\u09AF\u09BC\u0982\u0995\u09CD\u09B0\
  \u09BF\u09AF\u09BC \u0995\u09B0\u09A4\u09C7\u0964."
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

## কি এবং কেন?

HTTP অনুরোধ পাঠানো হল ওয়েব সার্ভারের সাথে যোগাযোগ করার একটি উপায়, যা দ্বারা ডেটা পুনরুদ্ধার করা বা ফর্ম জমা দেওয়া হয়। প্রোগ্রামাররা এটি করেন ওয়েব সার্ভিস, API-এর সাথে যোগাযোগ করতে বা ওয়েব সামগ্রী সংক্রান্ত কাজ স্বয়ংক্রিয় করতে।

## কিভাবে:

Bash `curl` বা `wget` এর মতো টুল ব্যবহার করে HTTP অনুরোধ করতে পারে। এখানে `curl` দিয়ে একটি দ্রুত উদাহরণ দেওয়া হলো।

```Bash
# একটি ওয়েবপেজের বিষয়বস্তু পেতে
curl https://example.com

# সার্ভারে ডেটা পোস্ট করা
curl -d "param1=value1&param2=value2" -X POST https://example.com/post-endpoint

# একটি GET অনুরোধে হেডারগুলি অন্তর্ভুক্ত করা
curl -H "Content-Type: application/json" https://example.com
```

`curl` প্রতিক্রিয়ার নমুনা:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## গভীর ডুব

HTTP অনুরোধগুলি ৯০ এর দশকের প্রারম্ভে থেকে বিদ্যমান এবং ওয়েব যোগাযোগের মূলভিত্তি। `curl` এবং `wget` হল Unix কমান্ড লাইনের টুল, যা যথাক্রমে ১৯৯৬ এবং ১৯৯৬ সালে নেটওয়ার্ক অনুরোধের জন্য চালু করা হয়।

`wget` সাধারণত ফাইল ডাউনলোড করার জন্য ব্যবহৃত হয়, যখন `curl` বিভিন্ন প্রোটোকল সম্ভালতে পারে এবং আরও বৈশিষ্ট্য সরবরাহ করে, যা কমান্ড লাইন থেকে HTTP অনুরোধ পাঠানোর জন্য এটি একটি যাওয়া-আসা করার উপায়।

এই টুলগুলি ব্যবহার করে HTTP অনুরোধ বাস্তবায়নের জন্য সঠিক অনুরোধ হেডার, মেথড (GET, POST, PUT, DELETE, ইত্যাদি) এবং মাঝেমাঝে ডেটা পেলোডগুলি তৈরী করা জড়িত। Bash স্ক্রিপ্টের মাধ্যমে এটি করে ওয়েব-ভিত্তিক সেবাগুলির সাথে যোগাযোগের স্বয়ংক্রিয়তা সম্ভব করা যায়।

স্ক্রিপ্টিংয়ে HTTP অনুরোধ পাঠানোর বিকল্প উপায় গুলি পাইথনের মতো স্ক্রিপ্টিং ভাষাগুলি ব্যবহার করা যা `requests` এর মতো লাইব্রেরিগুলি সাথে থাকে, অথবা `httpie` মতো টুল ব্যবহার করা আরও মানুষের অনুকূল ইন্টারফেসের জন্য।

## আরো দেখুন

- curl অফিসিয়াল সাইট: https://curl.se/
- wget ম্যানুয়াল: https://www.gnu.org/software/wget/manual/wget.html
- HTTPie: https://httpie.io/
- The Bash Academy: https://www.bash.academy/
- W3C HTTP স্পেসিফিকেশন: https://www.w3.org/Protocols/
