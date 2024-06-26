---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:23.703618-06:00
description: ''
lastmod: '2024-04-05T22:51:05.382634-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কিভাবে:


### শুরুর/শেষের স্থান (হোয়াইটস্পেস) মুছে ফেলা:
```Bash
text="   হ্যালো, ওয়ার্ল্ড!   "
trimmed=$(echo "$text" | xargs)
echo "$trimmed"
```
আউটপুট: `হ্যালো, ওয়ার্ল্ড!`

### সকল সংখ্যা অক্ষর সরানো:
```Bash
text="B4sh i5 amaz1ng!"
cleaned=${text//[^a-zA-Z ]/}
echo "$cleaned"
```
আউটপুট: `Bsh i amazng`

### নির্দিষ্ট অক্ষর প্রতিস্থাপন:
```Bash
text="Hello-World!"
cleaned=${text//-/_}
echo "$cleaned"
```
আউটপুট: `Hello_World!`

## গভীর ডুব
শুরুতে, টেক্সট প্রক্রিয়াকরণ টুলস যেমন `sed` এবং `awk` ছিল স্ট্রিং ম্যানিপ্যুলেশনের জন্য সর্বপ্রথম পছন্দ। Bash এখন প্যাটার্ন ম্যাচিং এবং স্ট্রিং ম্যানিপ্যুলেশন সরাসরি শেলে নিয়ে এসেছে, এতে করে এর ব্যবহারকারীরা বাহ্যিক কমান্ডগুলির দরকার ছাড়াই প্রচুর ক্ষমতা পেয়ে যাচ্ছেন।

`${parameter/pattern/string}` সিনট্যাক্স হল একটি পদ্ধতি যেখানে আপনি `pattern` এর প্রথম মিলকে `string` এর সাথে প্রতিস্থাপন করতে পারেন। সকল মিল সরানোর জন্য, উদাহরণগুলিতে দেখানো অনুযায়ী আরেকটা `/` যোগ করুন।

বিকল্পগুলি ক্লাসিক UNIX টুলস যেমন `sed`, `awk`, `tr`, অথবা আরও আধুনিক স্ক্রিপ্টিং ভাষা যেমন Python অথবা Perl ব্যবহার করার মতো।

মৌলিকভাবে, Bash প্যাটার্ন ম্যাচিংয়ের জন্য গ্লোবিং এবং ওয়াইল্ডকার্ডস ব্যবহার করে, কিন্তু যখন আপনি সেই `${text//pattern/}` কনস্ট্রাক্টগুলি দেখতে পান, আপনি Bash-এর প্যারামিটার এক্সপ্যানশনের সাথে ডিল করছেন—একটি ফিচার যা স্ট্রিং ম্যানিপ্যুলেশনের জন্য আসলেই সুবিধাজনক।

## আরও দেখুন
- Bash Manual এ প্যারামিটার এক্সপ্যানশন সম্পর্কে: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- Linux-এ টেক্সট প্রক্রিয়াকরণ সম্পর্কিত একটি নিবন্ধ: https://www.linuxjournal.com/content/pattern-matching-bash
- Sed & Awk 101 Hacks ইবুক: https://www.thegeekstuff.com/ebooks/sed_awk_101_hacks
