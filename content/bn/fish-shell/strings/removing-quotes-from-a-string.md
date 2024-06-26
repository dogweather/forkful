---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:13:02.866771-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Fish \u098F\u09B0 \u098F \u09A7\
  \u09B0\u09A8\u09C7\u09B0 \u0995\u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u0985\u09AD\u09CD\u09AF\u09A8\u09CD\u09A4\u09B0\u09C0\u09A3 \u099C\u09BE\u09A6\
  \u09C1 \u0986\u099B\u09C7\u0964 `string` \u09AB\u09BE\u0982\u09B6\u09A8\u099F\u09BF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8 \u0998\u09BE\
  \u09AE \u099B\u09BE\u09A1\u09BC\u09BE\u0987\u0964 \u098F\u0987 \u09AE\u09A8\u09CD\
  \u09A4\u09CD\u09B0\u0997\u09C1\u09B2\u09BF \u09A6\u09C7\u0996\u09C1\u09A8."
lastmod: '2024-03-17T18:47:44.483572-06:00'
model: gpt-4-0125-preview
summary: "Fish \u098F\u09B0 \u098F \u09A7\u09B0\u09A8\u09C7\u09B0 \u0995\u09BE\u099C\
  \u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0985\u09AD\u09CD\u09AF\u09A8\u09CD\u09A4\
  \u09B0\u09C0\u09A3 \u099C\u09BE\u09A6\u09C1 \u0986\u099B\u09C7\u0964 `string` \u09AB\
  \u09BE\u0982\u09B6\u09A8\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C1\u09A8 \u0998\u09BE\u09AE \u099B\u09BE\u09A1\u09BC\u09BE\u0987\
  \u0964 \u098F\u0987 \u09AE\u09A8\u09CD\u09A4\u09CD\u09B0\u0997\u09C1\u09B2\u09BF\
  \ \u09A6\u09C7\u0996\u09C1\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\
  \u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE"
weight: 9
---

## কিভাবে:
Fish এর এ ধরনের কাজের জন্য অভ্যন্তরীণ জাদু আছে। `string` ফাংশনটি ব্যবহার করুন ঘাম ছাড়াই। এই মন্ত্রগুলি দেখুন:

```fish
# একক উদ্ধৃতি নিয়ে উদাহরণ
set quoted "'Hello, World!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # আউটপুট: Hello, World!

# দ্বৈত উদ্ধৃতি একই ব্যাপার
set double_quoted "\"Hello, Universe!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # আউটপুট: Hello, Universe!
```

## গভীরে যাওয়া
কমান্ড-লাইনের প্রাগৈতিহাসিক যুগে আপনি `sed` অথবা `awk` এর সাথে উদ্ধৃতি চিহ্ন অপসারণের জন্য কুস্তি করতেন; একটি বাস্তব জটিলতার গাঁথুনি এবং রহস্যজনক ফ্লাগের। Fish-এর `string` ফাংশনটি একটি নতুন যুগের থেকে, কোডকে পরিষ্কার এবং আরও সহজবোধ্য করে তোলে।

অন্যান্য শেলগুলিতে বিকল্পগুলি এখনও এই পুরানো টুলগুলির উপর নির্ভর করতে পারে অথবা ব্যাশের প্যারামিটার এক্সপ্যানশন বা zsh-এর মডিফায়ারগুলির মতো নিজস্ব অভ্যন্তরীণ পদ্ধতি ব্যবহার করতে পারে।

`string` ফাংশনটি শুধু উদ্ধৃতি চিহ্ন ট্রিমিং এর চেয়ে বেশি করে। এটি Fish এ স্ট্রিং অপারেশনের জন্য একটি সুইস আর্মি ছুরি। `string` এর সাথে, আপনি স্ট্রিং কাটা, কুঁচি করা, বিভাজন করা, যোগ করা, বা এমনকি টার্মিনালে সরাসরি রেগেক্স-ম্যাচ করতে পারেন।

## দেখুন
`string` নিয়ে আরও গভীরে ডুব দিন অফিসিয়াল ডকুমেন্টেশনের সাহায্যে:
- [Fish Shell স্ট্রিং ডকুমেন্টেশন](https://fishshell.com/docs/current/commands.html#string)

পুরানো দিনের জন্য নস্টালজিয়া অথবা ঐতিহ্যবাহী শেলগুলিতে স্ক্রিপ্টিং করার সময়, দেখুন:
- [Sed & Awk গাইড](https://www.grymoire.com/Unix/Sed.html)
- [Bash প্যারামিটার এক্সপানশন](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
