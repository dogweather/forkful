---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:32.406667-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF \u099C\
  \u09CB\u09A1\u09BC\u09BE \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u09B9\u09B2\u09CB '\u09A4\u09BE\u09A6\u09C7\u09B0\u0995\u09C7 \u09B6\u09C7\
  \u09B7 \u09A5\u09C7\u0995\u09C7 \u09B6\u09C7\u09B7 \u09AA\u09B0\u09CD\u09AF\u09A8\
  \u09CD\u09A4 \u098F\u0995\u09A4\u09CD\u09B0\u09BF\u09A4 \u0995\u09B0\u09BE' \u098F\
  \u0995\u099F\u09BF \u0986\u09A1\u09BC\u09AE\u09CD\u09AC\u09B0\u09AA\u09C2\u09B0\u09CD\
  \u09A3 \u0989\u09AA\u09BE\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\
  \u09A8 \u09B6\u09AC\u09CD\u09A6 \u098F\u09AC\u0982 \u09AC\u09BE\u0995\u09CD\u09AF\
  \ \u098F\u0995\u09C0\u09AD\u09C2\u09A4 \u0995\u09B0\u09A4\u09C7,\u2026"
lastmod: '2024-03-17T18:47:44.579084-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF \u099C\
  \u09CB\u09A1\u09BC\u09BE \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u09B9\u09B2\u09CB '\u09A4\u09BE\u09A6\u09C7\u09B0\u0995\u09C7 \u09B6\u09C7\
  \u09B7 \u09A5\u09C7\u0995\u09C7 \u09B6\u09C7\u09B7 \u09AA\u09B0\u09CD\u09AF\u09A8\
  \u09CD\u09A4 \u098F\u0995\u09A4\u09CD\u09B0\u09BF\u09A4 \u0995\u09B0\u09BE' \u098F\
  \u0995\u099F\u09BF \u0986\u09A1\u09BC\u09AE\u09CD\u09AC\u09B0\u09AA\u09C2\u09B0\u09CD\
  \u09A3 \u0989\u09AA\u09BE\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\
  \u09A8 \u09B6\u09AC\u09CD\u09A6 \u098F\u09AC\u0982 \u09AC\u09BE\u0995\u09CD\u09AF\
  \ \u098F\u0995\u09C0\u09AD\u09C2\u09A4 \u0995\u09B0\u09A4\u09C7,\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
স্ট্রিংগুলি জোড়া দেওয়া মানে হলো 'তাদেরকে শেষ থেকে শেষ পর্যন্ত একত্রিত করা' একটি আড়ম্বরপূর্ণ উপায়। প্রোগ্রামাররা এটি করেন শব্দ এবং বাক্য একীভূত করতে, বার্তা তৈরি করতে, অথবা গতানুগতিকভাবে মান টেক্সটে সন্নিবেশ করার জন্য।

## কিভাবে:
রুবিতে, আপনি `+` অপারেটর অথবা `<<` মেথড দ্বারা স্ট্রিংগুলি জোড়া দিতে পারেন, যা স্ট্রিংটিকে জায়গায় পরিবর্তন করে। এইভাবে বিন্দুগুলি, বা বরং, শব্দগুলি একত্রিত করুন:

```Ruby
# + অপারেটর ব্যবহার করে, যা একটি নতুন স্ট্রিং ফেরত দেয়
greeting = "Hello, " + "world!"
puts greeting # আউটপুট: Hello, world!

# << মেথড ব্যবহার করে, যা মূল স্ট্রিংটি পরিবর্তন করে
name = "Alice"
name << ", meet Bob"
puts name # আউটপুট: Alice, meet Bob
```

## গভীরে ডুব
রুবিতে যোগ করা তার জন্ম থেকেই আছে। কিন্তু সময়ের সাথে সাথে, ভাষাটি স্ট্রিংগুলি একত্রে বুনতে আরও উপায় সরবরাহ করেছে।

আমরা `+` এবং `<<` নিয়ে আলোচনা করেছি, তবে এছাড়াও `concat` মেথড এবং ইন্টারপোলেশন আছে।

- `concat` ব্যবহার করে: এই পদ্ধতি `<<` এর মতো কিন্তু এটি আপনাকে একসাথে একাধিক স্ট্রিং যোগ করতে দেয়।
```Ruby
phrase = "Roses are red"
phrase.concat(", violets are blue")
puts phrase # আউটপুট: Roses are red, violets are blue
```

- ইন্টারপোলেশন: সরাসরি যোগ করা ছাড়াই স্ট্রিং-এ ভ্যারিয়েবল প্রবেশ করানো। এটি পরিচ্ছন্ন এবং ভ্যারিয়েবল সন্নিবেশের জন্য প্রাধান্যযুক্ত:
```Ruby
mood = "excited"
message = "I am #{mood} to learn Ruby!"
puts message # আউটপুট: I am excited to learn Ruby!
```

ইন্টারপোলেশন স্বয়ংক্রিয়ভাবে যেকোনো ভ্যারিয়েবলের উপর `to_s` কল করে, সুনিশ্চিত করে যে অ স্ট্রিং প্রকারের সাথে স্ট্রিং-এর মধ্যে ঠিকঠাক মিলিত হয়।

আরও, মনে রাখবেন—এটি শুধুমাত্র শব্দগুলি একত্রিত করা নয়; রুবি কার্যকারিতা উপরও নজর রাখে। আপনি যখন `+` ব্যবহার করেন, রুবি একটি নতুন স্ট্রিং তৈরি করে। সময়ের সাথে বা লুপের মধ্যে এটি মেমোরি-খাদ্য হতে পারে। বিপরীতভাবে, `<<` এবং `concat` মূল স্ট্রিংটি পরিবর্তন করে, যা প্রায়শই আরও দক্ষ।

## আরও দেখুন
- স্ট্রিং উপর রুবি ডকুমেন্টেশন: https://ruby-doc.org/core-3.1.2/String.html
- রুবি স্ট্রিং ইন্টারপোলেশন নিয়ে একটি নিবন্ধ: https://www.rubyguides.com/2018/11/ruby-string-interpolation/
- রুবি অপারেটরগুলি নিয়ে একটি গাইড: https://www.tutorialspoint.com/ruby/ruby_operators.htm
