---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:38.578192-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u0995\u09C7 \u0995\u09CD\u09AF\u09BE\u09AA\u09BF\u099F\u09BE\u09B2\u09BE\u0987\
  \u099C \u0995\u09B0\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 \u09B9\u099A\u09CD\u099B\
  \u09C7 \u098F\u099F\u09BF \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\
  \u09B0\u09BE \u09AF\u09BE\u09A4\u09C7 \u09AA\u09CD\u09B0\u09A5\u09AE \u0985\u0995\
  \u09CD\u09B7\u09B0\u099F\u09BF \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\u09C7\u09B0\
  \ (\u0986\u09AA\u09BE\u09B0\u0995\u09C7\u09B8) \u09B9\u09AF\u09BC \u098F\u09AC\u0982\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u099F\u09BF\u09B0 \u09AC\u09BE\u0995\
  \u09BF \u0985\u0982\u09B6\u099F\u09BF \u099B\u09CB\u099F \u09B9\u09BE\u09A4\u09C7\
  \u09B0 (\u09B2\u09CB\u09AF\u09BC\u09BE\u09B0\u0995\u09C7\u09B8)\u2026"
lastmod: '2024-03-17T18:47:44.478695-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0995\
  \u09C7 \u0995\u09CD\u09AF\u09BE\u09AA\u09BF\u099F\u09BE\u09B2\u09BE\u0987\u099C\
  \ \u0995\u09B0\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 \u09B9\u099A\u09CD\u099B\u09C7\
  \ \u098F\u099F\u09BF \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\
  \u09BE \u09AF\u09BE\u09A4\u09C7 \u09AA\u09CD\u09B0\u09A5\u09AE \u0985\u0995\u09CD\
  \u09B7\u09B0\u099F\u09BF \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\u09C7\u09B0 (\u0986\
  \u09AA\u09BE\u09B0\u0995\u09C7\u09B8) \u09B9\u09AF\u09BC \u098F\u09AC\u0982 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982\u099F\u09BF\u09B0 \u09AC\u09BE\u0995\u09BF\
  \ \u0985\u0982\u09B6\u099F\u09BF \u099B\u09CB\u099F \u09B9\u09BE\u09A4\u09C7\u09B0\
  \ (\u09B2\u09CB\u09AF\u09BC\u09BE\u09B0\u0995\u09C7\u09B8)\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

একটি স্ট্রিং কে ক্যাপিটালাইজ করার অর্থ হচ্ছে এটি পরিবর্তন করা যাতে প্রথম অক্ষরটি বড় হাতের (আপারকেস) হয় এবং স্ট্রিংটির বাকি অংশটি ছোট হাতের (লোয়ারকেস) হয়। টেক্সট প্রসেসিং, ব্যবহারকারীদের ইনপুট নরমালাইজেশন, এবং ডাটা ফর্ম্যাটিং নিশ্চিত করা বা নির্দিষ্ট ফর্ম্যাটিং মানদণ্ডগুলি পূরণ করার জন্য এটি একটি সাধারণ কাজ।

## কিভাবে:

Fish Shell এ, স্ট্রিংগুলি সরাসরি বিল্ট-ইন ফাংশনগুলির সাথে ম্যানিপুলেট করা যায়, বাহ্যিক টুলস বা লাইব্রেরিগুলির দরকার ছাড়াই। একটি স্ট্রিং কে ক্যাপিটালাইজ করতে, আপনি `string` কমান্ড এবং উপ-কমান্ড গুলি একত্রিত করতে পারেন।

```fish
# নমুনা স্ট্রিং
set sample_string "hello world"

# প্রথম অক্ষরটি ক্যাপিটালাইজ করুন
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

আউটপুট:
```
Hello world
```

এমন সেনারিওগুলির জন্য যেখানে একটি স্ট্রিং এর একাধিক শব্দের ক্যাপিটালাইজেশনের প্রয়োজন (উদাঃ "hello world" কে "Hello World" এ রূপান্তর), আপনি প্রতিটি শব্দের উপর ইটারেট করে, প্রতিটিতে ক্যাপিটালাইজেশন লজিক প্রয়োগ করবেন:

```fish
# নমুনা বাক্য
set sentence "hello fish shell programming"

# প্রতিটি শব্দ ক্যাপিটালাইজ করুন
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# ক্যাপিটালাইজড শব্দগুলি জয়েন করুন
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

আউটপুট:
```
Hello Fish Shell Programming
```

লক্ষ্য করুন, Fish Shell সরাসরি পুরো বাক্যের ক্যাপিটালাইজেশনের জন্য একক-কমান্ড পদ্ধতি অফার করে না, যেভাবে কিছু প্রোগ্রামিং ভাষা তাদের স্ট্রিং মেথডগুলির সাথে করে। তাই, `string split`, `string sub`, `string upper` এবং তারপর পুনরায় জয়েনিং করা Fish Shell এ এই অর্জনের জন্য একটি ইডিওম্যাটিক পদ্ধতি প্রতিনিধিত্ব করে।
