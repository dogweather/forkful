---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:17:15.180099-06:00
description: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09AE\u09BE\u09A8\u09C7 \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\
  \u09CD\u09AD\u09BE\u09B0 \u09A5\u09C7\u0995\u09C7 \u09A1\u09C7\u099F\u09BE \u0986\
  \u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\
  \u09B8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u200D\u09CD\u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09A4\u09C7, \u09A4\u09A5\
  \u09CD\u09AF \u09B8\u0982\u0997\u09CD\u09B0\u09B9 \u0995\u09B0\u09A4\u09C7 \u09AC\
  \u09BE \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u09AE\u09A7\u09CD\
  \u09AF\u09C7 \u09AF\u09CB\u0997\u09BE\u09AF\u09CB\u0997\u2026"
lastmod: '2024-03-17T18:47:44.362130-06:00'
model: gpt-4-0125-preview
summary: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09AE\u09BE\u09A8\u09C7 \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\
  \u09CD\u09AD\u09BE\u09B0 \u09A5\u09C7\u0995\u09C7 \u09A1\u09C7\u099F\u09BE \u0986\
  \u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\
  \u09B8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u200D\u09CD\u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09A4\u09C7, \u09A4\u09A5\
  \u09CD\u09AF \u09B8\u0982\u0997\u09CD\u09B0\u09B9 \u0995\u09B0\u09A4\u09C7 \u09AC\
  \u09BE \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u09AE\u09A7\u09CD\
  \u09AF\u09C7 \u09AF\u09CB\u0997\u09BE\u09AF\u09CB\u0997\u2026"
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
HTTP অনুরোধ পাঠানো মানে ওয়েব সার্ভার থেকে ডেটা আনা। প্রোগ্রামাররা ওয়েব সার্ভিসের সাথে ইন্টার‍্যাক্ট করতে, তথ্য সংগ্রহ করতে বা সিস্টেমের মধ্যে যোগাযোগ করতে এটি করে থাকেন।

## কিভাবে:

```C++
#include <iostream>
#include <cpr/cpr.h> // প্রথমে CPR লাইব্রেরি ইন্সটল করা নিশ্চিত করুন।

int main() {
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/get"});
    std::cout << r.text << std::endl; // উত্তর শরীর দেখায়।
    return 0;
}
```

নমুনা আউটপুট:
```json
{
  "args": {},
  "headers": {
    "Accept": "*/*",
    "Host": "httpbin.org",
    "User-Agent": "curl/7.64.1"
  },
  "origin": "0.0.0.0",
  "url": "https://httpbin.org/get"
}
```

## গভীরে যাওয়া
HTTP অনুরোধ ওয়েবের আবির্ভাবের পর থেকে অপরিহার্য হয়েছে; এটি একটি ক্লায়েন্ট-সার্ভার মডেল অনুসরণ করে। CPR মতো C++ লাইব্রেরিগুলোর আগে, HTTP অনুরোধ পাঠানো সাধারণত `libcurl` সরাসরি ব্যবহার করা মানে, অথবা ওয়েব যোগাযোগের জন্য অধিক সজ্জিত অন্য কোন ভাষা সহ ইন্টিগ্রেট করা।

CPR, যার অর্থ C++ Requests, এটি Python এর `requests` মডিউল দ্বারা অনুপ্রাণিত একটি সহজে ব্যবহার করা যায় এমন একটি র‍্যাপার। এটি `libcurl` এর অনেক জটিলতাকে সরাসরি অ্যাবসট্রাক্ট করে। অল্টারনেটিভগুলি এবং অস্তিত্বে আছে, যেমন লোয়ার-লেভেল HTTP/S অপারেশনের জন্য Boost.Beast বা POCO লাইব্রেরিগুলি পোর্টেবিলিটি অফার করে।

চালকের আসনের নিচে গিয়ে, HTTP অনুরোধ পাঠানো মানে একটি TCP সংযোগ সেটআপ করা, HTTP প্রোটোকলের সাথে সামঞ্জস্যপূর্ণ একটি অনুরোধ বিন্যাসকরণ করা, তারপর উত্তরটি পার্স করা। শুরু থেকে এটি সঠিকভাবে পাওয়া অ-সামান্য কারণ ত্রুটি হাতলানো, HTTP সংস্করণ জটিলতা, এবং নিরাপত্তা বিবেচনাগুলি জড়িত।

## আরও দেখুন

- CPR Github রেপোজিটরি: https://github.com/libcpr/cpr
- `libcurl` ডকুমেন্টেশন আরও উন্নত ব্যবহারের জন্য: https://curl.se/libcurl/
- অফিসিয়াল Boost.Beast ডকুমেন্টেশন: https://www.boost.org/doc/libs/release/libs/beast/
- POCO C++ লাইব্রেরিগুলি ডকুমেন্টেশন: https://pocoproject.org/docs/
