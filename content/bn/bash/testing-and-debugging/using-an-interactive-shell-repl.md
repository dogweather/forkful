---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:22:50.675179-06:00
description: "REPL \u09B9\u09B2 Read-Eval-Print Loop-\u098F\u09B0 \u09B8\u0982\u0995\
  \u09CD\u09B7\u09BF\u09AA\u09CD\u09A4 \u09B0\u09C2\u09AA, \u09AF\u09BE \u098F\u0995\
  \u099F\u09BF \u09B8\u09B9\u099C, \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\
  \u09AF\u09BE\u0995\u09CD\u099F\u09BF\u09AD \u0995\u09AE\u09CD\u09AA\u09BF\u0989\u099F\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\
  \ \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\u0964 \u0995\u09CB\u09A1\u09BE\u09B0\u09B0\
  \u09BE \u098F\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u0995\u09CB\u09A1 \u09B2\
  \u09C7\u0996\u09BE \u098F\u09AC\u0982 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE\
  \ \u0995\u09B0\u09BE,\u2026"
lastmod: '2024-03-17T18:47:44.228502-06:00'
model: gpt-4-0125-preview
summary: "REPL \u09B9\u09B2 Read-Eval-Print Loop-\u098F\u09B0 \u09B8\u0982\u0995\u09CD\
  \u09B7\u09BF\u09AA\u09CD\u09A4 \u09B0\u09C2\u09AA, \u09AF\u09BE \u098F\u0995\u099F\
  \u09BF \u09B8\u09B9\u099C, \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\u09AF\
  \u09BE\u0995\u09CD\u099F\u09BF\u09AD \u0995\u09AE\u09CD\u09AA\u09BF\u0989\u099F\u09BE\
  \u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AA\
  \u09B0\u09BF\u09AC\u09C7\u09B6\u0964 \u0995\u09CB\u09A1\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u0995\u09CB\u09A1 \u09B2\u09C7\
  \u0996\u09BE \u098F\u09AC\u0982 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\
  \u09B0\u09BE, \u09B8\u09BF\u09A8\u099F\u09CD\u09AF\u09BE\u0995\u09CD\u09B8 \u09A8\
  \u09BF\u09AF\u09BC\u09C7 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE, \u098F\u09AC\u0982 \u09AA\u09C1\u09B0\u09CB \u0985\u09CD\u09AF\u09BE\u09AA\
  \u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8 \u09A4\u09C8\u09B0\u09BF \u098F\u09AC\
  \u0982 \u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE\u09B0 \u09AC\u09BE\u09A1\
  \u09BC\u09A4\u09BF \u0995\u09B7\u09CD\u099F \u099B\u09BE\u09A1\u09BC\u09BE\u0987\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09A7\u09BE\
  \u09B0\u09A3\u09BE \u09B6\u09BF\u0996\u09A4\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7\u0964."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কি এবং কেন?
REPL হল Read-Eval-Print Loop-এর সংক্ষিপ্ত রূপ, যা একটি সহজ, ইন্টার‌্যাক্টিভ কম্পিউটার প্রোগ্রামিং পরিবেশ। কোডাররা এটি দ্রুত কোড লেখা এবং পরীক্ষা করা, সিনট্যাক্স নিয়ে পরীক্ষা করা, এবং পুরো অ্যাপ্লিকেশন তৈরি এবং চালনা করার বাড়তি কষ্ট ছাড়াই প্রোগ্রামিং ধারণা শিখতে ব্যবহার করে।

## কিভাবে:
Bash-এ, আপনার টার্মিনাল মূলত একটি REPL হয়। আপনি একটি কমান্ড টাইপ করেন; এটি তা পড়ে, মূল্যায়ন করে, ফলাফল প্রিন্ট করে, এবং আপনার পরবর্তী কমান্ডের জন্য অপেক্ষা করে লুপে ফিরে যায়। এখানে বাশ কে REPL হিসেবে ব্যবহার করার একটি উদাহরণ:

```Bash
$ echo "Hello, World!"
Hello, World!
$ x=$((6 * 7))
$ echo $x
42
```

আপনার ইনপুট `$ ` প্রম্পটের পরে অনুসরণ করে, আউটপুট পরের লাইনে প্রিন্ট করা হয়। সহজ, তাই না?

## গভীরে ডুব দিন
Bash, যা Bourne Again SHell এর সংক্ষিপ্ত রূপ, অনেক Unix-ভিত্তিক সিস্টেমে ডিফল্ট শেল। এটি মূল Bourne shell-এর একটি আপগ্রেড, যা ১৯৭০-এর শেষে নির্মিত হয়েছিল। Bash একটি শক্তিশালী স্ক্রিপ্টিং টুল হলেও, এর ইন্টার‌্যাক্টিভ মোড আপনাকে লাইন অনুযায়ী কমান্ড চালাতে দেয়।

বিকল্প বিবেচনা করে দেখতে গেলে, আপনার কাছে পাইথন REPL আছে (কেবল আপনার টার্মিনালে `python` টাইপ করুন), Node.js (সাথে `node`), এবং IPython, একটি উন্নত ইন্টার‌্যাক্টিভ পাইথন শেল। প্রতিটি ভাষার প্রায়ই তার নিজস্ব REPL বাস্তবায়ন থাকে।

অধীনে, REPLs হল লুপ যা আপনার ইনপুটকে (কমান্ড বা কোড) পার্স করে, চালায় এবং stdout (আপনার স্ক্রিন) তে ফলাফল ফেরত দেয়, প্রায়শই ভাষার ইন্টারপ্রেটারকে সরাসরি ব্যবহার করে। শিক্ষা এবং প্রোটোটাইপিং করার জন্য এই তাত্ক্ষণিক ফিডব্যাক অসামান্য।

## আরও দেখুন
- [অফিসিয়াল GNU Bash ডকুমেন্টেশন](https://gnu.org/software/bash/manual/bash.html)
- [Learn Shell ইন্টার‌্যাক্টিভ টিউটোরিয়াল](https://www.learnshell.org/)
- [IPython অফিসিয়াল ওয়েবসাইট](https://ipython.org/)
- [REPL.it](https://replit.com/): একটি বহুভাষী অনলাইন REPL (শুধুমাত্র Bash নয়!)
