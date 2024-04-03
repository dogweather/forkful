---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:50.688886-06:00
description: "\u09AC\u09CD\u09AF\u09BE\u09B6\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u099C\u09CB\u09A1\u09BC\u09BE \u09AC\u09B2\u09A4\u09C7 \u09A6\u09C1\u099F\
  \u09BF \u0985\u09A5\u09AC\u09BE \u09A4\u09A4\u09CB\u09A7\u09BF\u0995 \u099F\u09C7\
  \u0995\u09CD\u09B8\u099F \u09AA\u09B0\u09CD\u09AC \u098F\u0995\u09B8\u09BE\u09A5\
  \u09C7 \u099C\u09CB\u09A1\u09BC\u09BE \u09A6\u09C7\u09AF\u09BC\u09BE\u0995\u09C7\
  \ \u09AC\u09C1\u099D\u09BE\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0995\u09AE\u09BE\u09A8\u09CD\u09A1\
  \ \u09A4\u09C8\u09B0\u09BF, \u09AB\u09BE\u0987\u09B2 \u09AA\u09BE\u09A5 \u09A8\u09BF\
  \u09B0\u09CD\u09AE\u09BE\u09A3, \u0985\u09A5\u09AC\u09BE \u0995\u09C7\u09AC\u09B2\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F \u099F\u09C7\u0995\u09CD\u09B8\u099F\u2026"
lastmod: '2024-03-17T18:47:44.215517-06:00'
model: gpt-4-0125-preview
summary: "\u09AC\u09CD\u09AF\u09BE\u09B6\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u099C\u09CB\u09A1\u09BC\u09BE \u09AC\u09B2\u09A4\u09C7 \u09A6\u09C1\u099F\
  \u09BF \u0985\u09A5\u09AC\u09BE \u09A4\u09A4\u09CB\u09A7\u09BF\u0995 \u099F\u09C7\
  \u0995\u09CD\u09B8\u099F \u09AA\u09B0\u09CD\u09AC \u098F\u0995\u09B8\u09BE\u09A5\
  \u09C7 \u099C\u09CB\u09A1\u09BC\u09BE \u09A6\u09C7\u09AF\u09BC\u09BE\u0995\u09C7\
  \ \u09AC\u09C1\u099D\u09BE\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0995\u09AE\u09BE\u09A8\u09CD\u09A1\
  \ \u09A4\u09C8\u09B0\u09BF, \u09AB\u09BE\u0987\u09B2 \u09AA\u09BE\u09A5 \u09A8\u09BF\
  \u09B0\u09CD\u09AE\u09BE\u09A3, \u0985\u09A5\u09AC\u09BE \u0995\u09C7\u09AC\u09B2\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\
  \u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
weight: 3
---

## কিভাবে:
এখানে ব্যাশে আপনার স্ট্রিংগুলিকে দ্রুত জড়িয়ে দেয়ার উপায়ের দ্রুত পথ:

```Bash
# স্ট্রিংগুলিকে একে অপরের পাশে রেখে জোড়া
greeting="Hello, "
name="world!"
welcome=$greeting$name
echo $welcome  # আউটপুট: Hello, world!

# স্পষ্টতার জন্য কার্লি ব্রেসিস ব্যবহার
version="version"
number=1
full_version=${version}_${number}
echo $full_version  # আউটপুট: version_1

# ভ্যারিয়েবল এবং লিটারালের সাথে জোড়া
timestamp=$(date +%Y%m%d)  # বর্তমান তারিখ পান YYYYMMDD ফরম্যাটে
filename="backup_${timestamp}.tar.gz"
echo $filename  # আউটপুট: backup_20230315.tar.gz
```

## গভীর ডাইভ
GUIs এর রাজত্ব শুরু হওয়ার আগে, কমান্ড লাইন এবং স্ক্রিপ্টগুলি ছিল কম্পিউটার ইন্টার‌্যাকশনের রাজা। স্ট্রিং জোড়া সবসময় অপরিহার্য ছিল, কারণ এটি ডাইনামিক কমান্ড এবং ফাইল ম্যানিপুলেশনের অনুমতি দেয়।

তারিখে একটি ঐতিহাসিক বিকল্প `expr` কমান্ড ছিল, যা এখন একটি পুরাতন বস্তু মনে হয়:

```Bash
older_way=$(expr $greeting $name)
```

কিন্তু ব্যাশ বলে, "সেই ঝামেলা কারো দরকার নেই?" এবং এটিকে স্বাভাবিক করে তুলেছে। কিভাবে? ভালো, ব্যাশ স্ট্রিংগুলিকে যেন আদরের বন্ধু হিসেবে দেখে: তাদের একে অপরের পাশে রাখো এবং তারা একসাথে মিলিত হয়ে যায়।

ব্যাপারটি হল, ব্যাশ এর অধীনে কোন বিশেষ ফাংশন অথবা সিনট্যাক্স ছাড়াই এই জোড়া হ্যান্ডেল করে। শব্দগুলি অথবা ভ্যারিয়েবলগুলি কেবল একসাথে মিশে যায়। তবে, যদি আপনার ভ্যারিয়েবলগুলি একটি নম্বর অথবা একটি আন্ডারস্কোর দিয়ে শুরু হয়, তাহলে সাধারণত অন্য ভ্যারিয়েবল নামের সাথে বিভ্রান্তি এড়াতে আপনি তাদের কার্লি ব্রেসিসে মোড়ানোর কথা ভাববেন।

তবে, একটি ধরা আছে: স্পেস বিষয়টি গুরুত্বপূর্ণ। যদি আপনি সতর্ক না হন, তাহলে আপনি অনাকাঙ্ক্ষিত ফাঁক অথবা একটি একসাথে চেপে বসা একটি জিনিস পেতে পারেন।

একটি বর্তমান বিকল্প হল `printf` ফাংশন ব্যবহার করা, যা আপনাকে ফরম্যাটিং নিয়ন্ত্রণ দেয়:

```Bash
printf -v full_greeting "%s%s" "$greeting" "$name"
echo $full_greeting  # আউটপুট: Hello, world!
```

## দেখুন আরও
- [GNU Bash ম্যানুয়াল](https://www.gnu.org/software/bash/manual/) ব্যাশের সকল জিনিসের মূল বুঝতে।
- [আডভান্সড ব্যাশ-স্ক্রিপ্টিং গাইড](https://tldp.org/LDP/abs/html/) স্ক্রিপ্টিং জিমন্যাস্টিক্স এবং আরও উদাহরণের জন্য।
