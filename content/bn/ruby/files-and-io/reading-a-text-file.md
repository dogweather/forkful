---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:11.563232-06:00
description: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\
  \u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u0995\u09CB\u09A1\u09C7\u09B0 \u09AE\
  \u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09A1\u09BF\u09B8\u09CD\u0995\u09C7 \u09B8\
  \u0982\u09B0\u0995\u09CD\u09B7\u09BF\u09A4 \u098F\u0995\u099F\u09BF \u09AB\u09BE\
  \u0987\u09B2\u09C7\u09B0 \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u0985\u09CD\u09AF\u09BE\u0995\u09CD\u09B8\u09C7\
  \u09B8 \u09AA\u09C7\u09A4\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09BE\u09A6\u09C7\u09B0 \u0985\u09CD\
  \u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u09AE\u09A7\
  \u09CD\u09AF\u09C7 \u09A1\u09BE\u099F\u09BE \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\
  \u09BF\u09AF\u09BC\u09BE,\u2026"
lastmod: '2024-03-17T18:47:44.608926-06:00'
model: gpt-4-0125-preview
summary: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u0995\u09CB\u09A1\u09C7\u09B0 \u09AE\u09BE\
  \u09A7\u09CD\u09AF\u09AE\u09C7 \u09A1\u09BF\u09B8\u09CD\u0995\u09C7 \u09B8\u0982\
  \u09B0\u0995\u09CD\u09B7\u09BF\u09A4 \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0987\
  \u09B2\u09C7\u09B0 \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u0985\u09CD\u09AF\u09BE\u0995\u09CD\u09B8\u09C7\u09B8\
  \ \u09AA\u09C7\u09A4\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09BE\u09A6\u09C7\u09B0 \u0985\u09CD\u09AF\
  \u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u09AE\u09A7\u09CD\
  \u09AF\u09C7 \u09A1\u09BE\u099F\u09BE \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\
  \u09AF\u09BC\u09BE, \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3 \u0995\u09B0\
  \u09BE \u0985\u09A5\u09AC\u09BE \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8\
  \ \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF \u0995\u09B0\
  \u09C7 \u09A5\u09BE\u0995\u09C7\u09A8\u0964."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কিভাবে:
রুবি তে ফাইল পড়া খুবই সহজ। আপনি `File` ক্লাস ব্যবহার করতে পারেন, যা ফাইলগুলি পড়ার জন্য বিভিন্ন পদ্ধতি প্রদান করে। এখানে পুরো ফাইল পড়ার একটি সাধারণ উদাহরণ দেওয়া হল:

```Ruby
File.open("example.txt", "r") do |file|
  puts file.read
end
```

যদি `example.txt` টেক্সটে "Hello, Ruby!" থাকে, এখানে আপনি যা পাবেন:

```
Hello, Ruby!
```

লাইন অনুযায়ী পড়ার জন্য:

```Ruby
File.foreach("example.txt") { |line| puts line }
```

একই `example.txt`, এখন আউটপুট লাইন অনুযায়ী হবে:

```
Hello, Ruby!
```

## গভীর ডুব:
ঐতিহাসিকভাবে, ফাইল পড়া প্রোগ্রামিং ভাষাগুলির একটি মৌলিক বৈশিষ্ট্য হয়েছে, যা ফাইলসিস্টেমের সাথে মিথস্ক্রিয়াকে অনুমতি দেয়।

রুবিতে, আপনি বিভিন্ন টুল দিয়েও ফাইল পড়তে পারেন:

1. `IO` ক্লাস: লো-লেভেল ফাইল অপারেশনসের জন্য।
2. `readlines` পদ্ধতি: প্রতিটি লাইনকে একটি বিষয় হিসেবে নিয়ে পুরো ফাইলকে একটি এ্যারেতে লোড করে।
3. `File.read`: পুরো ফাইলটি একটি স্ট্রিং হিসেবে পড়ার জন্য দ্রুত পদ্ধতি।

এখানে একটি বিনিময়ের বিবেচনা করতে হবে: `File.read` ছোট ফাইলের জন্য পরিপাটি, কিন্তু এটি বড় ফাইলগুলির জন্য মেমরি জনিত অন্তরায় হতে পারে। সেই সময়ে লাইন অনুযায়ী বা টুকরো টুকরো পড়া মূল্যবান হয়ে ওঠে।

## দেখুন ও:
- রুবি ডক্সের `File` ক্লাস: [ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html)
- রুবিতে ফাইল পড়া সম্পর্কে স্ট্যাক ওভারফ্লো আলোচনা: [stackoverflow.com/questions/tagged/ruby+file-io](https://stackoverflow.com/questions/tagged/ruby+file-io)
