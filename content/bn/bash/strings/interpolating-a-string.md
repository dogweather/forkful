---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:50.635458-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 \u0986\u09AA\u09A8\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \u09AF\u09BC\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09AE\u09BE\u09A8 \u09AF\
  \u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\
  \u0964 \u0995\u09BE\u09B8\u09CD\u099F\u09AE \u09AE\u09C7\u09B8\u09C7\u099C \u09A4\
  \u09C8\u09B0\u09BF \u0995\u09B0\u09BE, \u0985\u099F\u09CB\u09AE\u09C7\u099F\u09C7\
  \u09A1 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u098F\u09AC\u0982 \u09B8\u09CD\u0995\
  \u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09B2\u09C7\u0996\u09BE\u09B0 \u09AE\u09A4\
  \ \u0995\u09BE\u099C\u09C7 \u098F\u099F\u09BF \u0996\u09C1\u09AC\u0987\u2026"
lastmod: '2024-03-17T18:47:44.209150-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 \u0986\u09AA\u09A8\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \u09AF\u09BC\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09AE\u09BE\u09A8 \u09AF\
  \u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\
  \u0964 \u0995\u09BE\u09B8\u09CD\u099F\u09AE \u09AE\u09C7\u09B8\u09C7\u099C \u09A4\
  \u09C8\u09B0\u09BF \u0995\u09B0\u09BE, \u0985\u099F\u09CB\u09AE\u09C7\u099F\u09C7\
  \u09A1 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u098F\u09AC\u0982 \u09B8\u09CD\u0995\
  \u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09B2\u09C7\u0996\u09BE\u09B0 \u09AE\u09A4\
  \ \u0995\u09BE\u099C\u09C7 \u098F\u099F\u09BF \u0996\u09C1\u09AC\u0987\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
স্ট্রিং ইন্টারপোলেশনের মাধ্যমে আপনি স্ট্রিংয়ের মধ্যে মান যুক্ত করতে পারেন। কাস্টম মেসেজ তৈরি করা, অটোমেটেড কমান্ড এবং স্ক্রিপ্ট লেখার মত কাজে এটি খুবই উপকারী।

## কিভাবে:
Bash স্ট্রিংগুলি ভেরিয়েবলের সাথে সুন্দরভাবে কাজ করে। একটি স্ট্রিং এর মধ্যে কিছু কার্লি ব্রেসেস্‌ সহ একটি ভেরিয়েবল রাখুন, এবং আপনি তৈরি।

```Bash
name="World"
greeting="Hello, ${name}!"
echo $greeting
```

আউটপুট:
```
Hello, World!
```

Bash বলে, "এটা লচকপূর্ণ রাখুন।" `name` পরিবর্তন করুন, এবং আপনার অভিবাদন অনুসারীও তাই।

```Bash
name="Bash Pros"
greeting="Hello, ${name}!"
echo $greeting
```

আউটপুট:
```
Hello, Bash Pros!
```

## গভীরে যাওয়া
পুরনো দিনে, প্রোগ্রামাররা স্ট্রিংগুলি যোগ করে আটকে দিত। এটি জটিল হয়ে উঠত। স্ট্রিং ইন্টারপোলেশন আরও পরিষ্কার, পাঠযোগ্য কোডের জন্য একটি সুপারহিরোর মতো এসেছিল।

Bash অন্য কিছু ভাষার মতো নয়, কেবল একটি ডলার চিহ্ন এবং কিছু ব্রেস। অন্যান্য ভাষায় এটি বিশেষ সিনট্যাক্স বা ফাংশন দিয়ে সাজানো। Bash-এ, এটি সব ব্রেস এবং মাঝে মাঝে এস্কেপ ক্যারেক্টার যদি আপনি আরও চমৎকার অনুভব করতে চান।

কিছু বিকল্প? নিশ্চয়ই, আপনি কনক্যাটেনেট করতে পারেন বা যদি আপনি কিছু জটিল কিছু না করেন তাহলে ব্রেস ছাড়া `echo` ব্যবহার করতে পারেন। কিন্তু কেন সেটল করবেন?

বাস্তবায়নের বিষয়ে, যখন Bash `${}` দেখে, এটি ভেরিয়েবলের মান নিয়ে তা প্রতিস্থাপন করে, কোনো প্রশ্ন ছাড়াই। এটি নিশ্চিত করে যে আপনি যা দেখছেন (আপনার কোডে) তা আপনি পাচ্ছেন (আপনার আউটপুটে)।

## আরও দেখুন
স্ট্রিং জাদু সম্পর্কে আরও জানার জন্য:

- Bash স্ট্রিং ম্যানিপুলেশন: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- এডভান্সড বাশ-স্ক্রিপ্টিং গাইড: https://tldp.org/LDP/abs/html/
- Stack Overflow (বাস্তব বিশ্বের সমস্যার জন্য ব্যবহারিক উদাহরণ): https://stackoverflow.com/questions/tagged/bash
