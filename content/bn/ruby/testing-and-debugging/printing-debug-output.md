---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:08:03.724571-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF\u09A4\
  \u09C7, `puts` \u098F\u09AC\u0982 `p` \u09B9\u09B2\u09C7\u09A8 \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u0995\u09A8\u09B8\u09CB\u09B2\u09C7 \u09A6\u09CD\u09B0\u09C1\u09A4\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AF\
  \u09C7\u09A4\u09C7 \u09B9\u09AC\u09C7 \u09AE\u09C7\u09A5\u09A1\u0997\u09C1\u09B2\
  \u09BF\u0964."
lastmod: '2024-03-17T18:47:44.593021-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF\u09A4\u09C7, `puts` \u098F\u09AC\u0982 `p` \u09B9\
  \u09B2\u09C7\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09A8\u09B8\u09CB\u09B2\
  \u09C7 \u09A6\u09CD\u09B0\u09C1\u09A4 \u0986\u0989\u099F\u09AA\u09C1\u099F\u09C7\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09AF\u09C7\u09A4\u09C7 \u09B9\u09AC\u09C7 \u09AE\
  \u09C7\u09A5\u09A1\u0997\u09C1\u09B2\u09BF\u0964."
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কিভাবে:
রুবিতে, `puts` এবং `p` হলেন আপনার কনসোলে দ্রুত আউটপুটের জন্য যেতে হবে মেথডগুলি।

```Ruby
def who_said_what
  quote = "To be or not to be"
  author = "Shakespeare"
  puts "Quote: #{quote}"
  p "Said by: #{author}"
end

who_said_what
```

নমুনা আউটপুট:

```
Quote: To be or not to be
"Said by: Shakespeare"
```

`puts` মেথড মানব-পাঠ্য আউটপুট প্রিন্ট করে, শেষে একটি নতুন লাইন যোগ করে। অন্যদিকে, `p` একটি আরো কাঁচা ফর্মে মান প্রিন্ট করে, যখন আপনার কিছু একটি স্ট্রিং কিনা তা দেখা দরকার হয় তখন এটি উপযোগী।

## গভীরে প্রবেশ
ফ্যান্সি IDE-গুলি আসার আগে, কনসোলে প্রিন্ট করা ছিল ডিবাগিং। এটি একটি পুরানো কিন্তু সোনালি কৌশল, বিশেষ করে যখন আপনি একটি ডিবাগার সেটআপ করার ওপরের ব্যয় এড়াতে চান।

বিকল্প হিসেবে, আপনি জটিল অবজেক্টগুলি সুন্দর ভাবে প্রিন্ট করার জন্য `pp` বা বাড়তি পাঠ্যগতমান এনহ্যান্স করার জন্য `awesome_print` মতো জেম লাইব্রেরিগুলি ব্যবহার করতে পারেন। যদি আপনার ডিবাগ আউটপুট খুব অধিক হচ্ছে, তবে ভারবোসিটির মাত্রা নিয়ন্ত্রণে রাখার জন্য একটি লগিং লাইব্রেরি বিবেচনা করুন।

বাস্তবায়নের দিক থেকে, `puts` এবং `p` লেখে `$stdout`-এ, রুবির একটি গ্লোবাল আই/ও স্ট্রিমে। প্রয়োজন হলে আউটপুট রিডিরেক্ট করা যায়। মনে রাখবেন, যদিও এই মেথডগুলি সুবিধাজনক, অত্যধিক ডিবাগ প্রিন্ট আপনার কনসোলকে জটিল করে তুলতে পারে এবং ডিবাগিংকে কঠিন করে তুলতে পারে।

## আরও দেখুন
- `Kernel#puts`-এর জন্য রুবি ডকুমেন্টেশন: https://ruby-doc.org/core/Kernel.html#method-i-puts
- `Kernel#p`-এর জন্য রুবি ডকুমেন্টেশন: https://ruby-doc.org/core/Kernel.html#method-i-p
- রুবিতে প্রিটি প্রিন্টিং গাইড: https://ruby-doc.org/stdlib/libdoc/pp/rdoc/PP.html
- ফ্যান্সি আউটপুটের জন্য অসাম প্রিন্ট জেম: https://rubygems.org/gems/awesome_print/
