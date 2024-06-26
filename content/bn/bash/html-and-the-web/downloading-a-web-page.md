---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:47.491486-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0987 \u0995\u09BE\u099C\
  \u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AF\u09C7\u09A4\u09C7 \u09B9\u09AC\u09C7\
  \ \u0995\u09CB\u09A8 \u099F\u09C1\u09B2\u09C7\u09B0 \u0995\u09BE\u099B\u09C7? `curl`\u0964\
  \ \u098F\u099F\u09BF \u098F\u0995\u099F\u09BF \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\
  \u09BE\u09B2\u09C0 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1-\u09B2\u09BE\u0987\u09A8\
  \ \u0987\u0989\u099F\u09BF\u09B2\u09BF\u099F\u09BF \u09AF\u09BE \u0993\u09AF\u09BC\
  \u09C7\u09AC \u09A5\u09C7\u0995\u09C7 \u09A1\u09C7\u099F\u09BE \u0986\u09B9\u09B0\
  \u09A3 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09B8\u09AC\u099A\
  \u09C7\u09AF\u09BC\u09C7 \u09B8\u09B9\u099C \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0\u09C7\u09B0\u2026"
lastmod: '2024-04-05T22:40:38.509259-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0987 \u0995\u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AF\
  \u09C7\u09A4\u09C7 \u09B9\u09AC\u09C7 \u0995\u09CB\u09A8 \u099F\u09C1\u09B2\u09C7\
  \u09B0 \u0995\u09BE\u099B\u09C7?"
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কিভাবে:
এই কাজের জন্য যেতে হবে কোন টুলের কাছে? `curl`। এটি একটি শক্তিশালী কমান্ড-লাইন ইউটিলিটি যা ওয়েব থেকে ডেটা আহরণ করে। এখানে সবচেয়ে সহজ ব্যবহারের উদাহরণ দেওয়া হল:

```Bash
curl https://example.com -o webpage.html
```

এই কমান্ডটি `example.com` এর HTML ডাউনলোড করে এবং এটিকে `webpage.html` নামে একটি ফাইলে লিখে। আউটপুট দেখুন:

```Bash
# নমুনা আউটপুট
  % মোট    % প্রাপ্ত % স্থানান্তরিত  গড় গতি   সময়    সময়     সময়  বর্তমান
                                 ডাউনলোড  আপলোড   মোট   খরচ    বাকি  গতি
100  1256  100  1256    0     0   6458      0 --:--:-- --:--:-- --:--:--  6497
```

আপনি কি ডাউনলোড করছেন তা সময় অনুযায়ী দেখতে চান? `-o` ফেলে দিন এবং ডাউনলোডটি সরাসরি আপনার কনসোলে প্রিন্ট হবে:

```Bash
curl https://example.com
```

## গভীর ডাইভ
`curl` 1997 সাল থেকে চালু আছে, ওয়েব অপারেশনের জন্য নিজের স্থান তৈরি করে নিয়েছে। ব্রাউজার ডাউনলোডের চেয়ে `curl` কেন? অটোমেশন এবং স্ক্রিপ্ট-বান্ধব। এটি ইন্টারঅ্যাক্টিভ নয় এবং ব্যাশ স্ক্রিপ্টগুলিতে সহজে সংযোজিত করা সম্ভব।

উল্লেখ্য বিকল্পগুলি: `wget`, আরেকটি কমান্ড-লাইন পাওয়ারহাউস যা রিকার্সিভভাবে ওয়েব পৃষ্ঠা ডাউনলোড করতে পারে। ভারী-দায়িত্ব স্ক্র্যাপিং বা একটি বাস্তব ব্রাউজার প্রসঙ্গ প্রয়োজন হলে, প্রোগ্রামাররা সেলেনিয়াম, প্যাপেটিয়ার, অথবা স্ক্র্যাপি এর মত টুলস ব্যবহার করেন।

`curl` এর প্রক্রিয়া সম্পর্কে: এটি HTTP এবং HTTPS থেকে FTP পর্যন্ত একাধিক প্রটোকল সমর্থন করে, এবং বিভিন্ন অপশন (--header, --cookie, --user-agent, ইত্যাদি) দিয়ে অনুরোধগুলি সূক্ষ্মভাবে সামঞ্জস্য করার জন্য। প্লাস, এটি সাধারণত ইউনিক্স-ভিত্তিক সিস্টেমগুলিতে পূর্ব-ইনস্টল করা হয়ে থাকে।

## আরও দেখুন
- কার্ল ডকুমেন্টেশন: https://curl.haxx.se/docs/manpage.html
- Wget ম্যানুয়াল: https://www.gnu.org/software/wget/manual/wget.html
- পাইথন দিয়ে ওয়েব স্ক্রাপিং ভূমিকা: https://realpython.com/python-web-scraping-practical-introduction/
