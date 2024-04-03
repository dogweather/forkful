---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:49.045090-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF-\u09A4\
  \u09C7, \u0986\u09AA\u09A8\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 \u09AD\u09C7\u09B0\
  \u09BF\u09AF\u09BC\u09C7\u09AC\u09B2 \u0985\u09A5\u09AC\u09BE \u098F\u0995\u09CD\
  \u09B8\u09AA\u09CD\u09B0\u09C7\u09B6\u09A8\u0995\u09C7 `#{}` \u098F\u09B0 \u09AE\
  \u09A7\u09CD\u09AF\u09C7 \u09B0\u09BE\u0996\u09AC\u09C7\u09A8 \u098F\u09AC\u0982\
  \ \u0986\u09AA\u09A8\u09BF \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u099A\u09BE\u09A8\
  \ \u09B8\u09C7\u0996\u09BE\u09A8\u09C7 \u098F\u099F\u09BF \u09A6\u09CD\u09AC\u09BF\
  -\u0989\u09A6\u09CD\u09A7\u09C3\u09A4 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  -\u098F \u09AB\u09C7\u09B2\u09C7 \u09A6\u09C7\u09AC\u09C7\u09A8\u0964 \u09AF\u09C7\
  \u09AE\u09A8."
lastmod: '2024-03-17T18:47:44.572815-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF-\u09A4\u09C7, \u0986\u09AA\u09A8\u09BF \u0986\u09AA\
  \u09A8\u09BE\u09B0 \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2 \u0985\
  \u09A5\u09AC\u09BE \u098F\u0995\u09CD\u09B8\u09AA\u09CD\u09B0\u09C7\u09B6\u09A8\u0995\
  \u09C7 `#{}` \u098F\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B0\u09BE\u0996\u09AC\
  \u09C7\u09A8 \u098F\u09AC\u0982 \u0986\u09AA\u09A8\u09BF \u09AF\u09C7\u0996\u09BE\
  \u09A8\u09C7 \u099A\u09BE\u09A8 \u09B8\u09C7\u0996\u09BE\u09A8\u09C7 \u098F\u099F\
  \u09BF \u09A6\u09CD\u09AC\u09BF-\u0989\u09A6\u09CD\u09A7\u09C3\u09A4 \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982-\u098F \u09AB\u09C7\u09B2\u09C7 \u09A6\u09C7\u09AC\
  \u09C7\u09A8\u0964 \u09AF\u09C7\u09AE\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কিভাবে:
রুবি-তে, আপনি আপনার ভেরিয়েবল অথবা এক্সপ্রেশনকে `#{}` এর মধ্যে রাখবেন এবং আপনি যেখানে চান সেখানে এটি দ্বি-উদ্ধৃত স্ট্রিং-এ ফেলে দেবেন। যেমন:

```Ruby
name = "Jesse"
greeting = "Hey there, #{name}!"
puts greeting # => Hey there, Jesse!
```

আপনি শুধু ভেরিয়েবলে সীমাবদ্ধ নন; যেকোনো রুবি কোড সেখানে যেতে পারে:

```Ruby
price_per_kg = 5
quantity = 2
puts "Your total is: $#{price_per_kg * quantity}" # => Your total is: $10
```

মনে রাখবেন, একক উদ্ধৃতি কাজ করবে না:

```Ruby
puts 'Hey there, #{name}!' # => Hey there, \#{name}!
```

## গভীর ডুব
আগের দিনে, আমরা `+` অথবা `<<` ব্যবহার করে স্ট্রিং এবং ভেরিয়েবলগুলিকে সংযুক্ত করতাম, যা দ্রুত জটিল হয়ে উঠতো।

```Ruby
email = "user" + "@" + "example.com"
```

রুবি-তে স্ট্রিং ইন্টারপোলেশন প্রবেশ করে, একটি আরো উন্নত উপায় যা টেক্সট এবং কোডকে মিশ্রিত করে। রুবি `#{}` এর মধ্যে যা কিছু আছে তা মূল্যায়ন করে এবং স্বয়ংক্রিয়ভাবে এটিকে একটি স্ট্রিং-এ রূপান্তর করে দেয়। স্ট্রিংগুলি রূপান্তর ও সংযুক্ত করার কাজ থেকে এটি যে পরিমাণ কর্ম বাঁচায় সেদিকে চিন্তা করুন:

```Ruby
"pi is roughly #{Math::PI.round(2)}"
```

রুবি একক নয়; অনেক ভাষায় এই সুবিধার নিজস্ব স্বাদ আছে। কিন্তু সতর্কতা: অন্যান্য ভাষার মতো নয়, রুবি এই জাদুকে কেবল দ্বি-উদ্ধৃত স্ট্রিংগুলি এবং নির্দিষ্ট অন্যান্য ক্ষেত্রে (যেমন ব্যাকটিক এবং সিম্বলগুলি) সীমাবদ্ধ রাখে। একক-উদ্ধৃতি শুধুমাত্র তার মধ্যে যা আছে তাই বের করে দেয়, কার্লি ব্র্যাকেটসহ।

## আরও দেখুন
- সিনট্যাক্স সম্পর্কে রুবি ডকুমেন্টেশন: [রুবি ডকস - সিনট্যাক্স](https://ruby-doc.org/core-3.1.2/doc/syntax/literals_rdoc.html#label-Strings)
- স্ট্রিং ম্যানিপুলেশন সম্পর্কে গভীর দৃষ্টি: [রুবি-ডক.অর্গ - স্ট্রিং](https://ruby-doc.org/core-3.1.2/String.html)
