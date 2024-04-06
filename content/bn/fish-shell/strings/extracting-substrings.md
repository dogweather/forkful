---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:47.987301-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AB\u09BF\u09B6\u09C7, \u0986\
  \u09AA\u09A8\u09BF `string` \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u09A8\u09BF\u09DF\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09C7\u09A8\u0964\
  \ \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2\
  \u09CB \u0995\u09BF\u09AD\u09BE\u09AC\u09C7."
lastmod: '2024-03-17T18:47:44.484536-06:00'
model: gpt-4-0125-preview
summary: "\u09AB\u09BF\u09B6\u09C7, \u0986\u09AA\u09A8\u09BF `string` \u0995\u09AE\
  \u09BE\u09A8\u09CD\u09A1 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A8\u09BF\u09DF\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\
  \u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2\u09CB \u0995\u09BF\u09AD\u09BE\u09AC\
  \u09C7."
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কিভাবে:
ফিশে, আপনি `string` কমান্ড ব্যবহার করে স্ট্রিং নিয়ে কাজ করেন। এখানে দেখানো হলো কিভাবে:

### শুরু থেকে নেওয়া:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -l 4 # 'Fish' আউটপুট দেয়
```

### শেষ থেকে কেটে নেওয়া:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -s -4 # 'fun!' আউটপুট দেয়
```

### নির্দিষ্ট পরিসীমা:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -s 6 -l 5 # 'Shell' আউটপুট দেয়
```

## গভীর ডাইভ
পুরনো দিনে, আমরা ফিশে স্ট্রিং কাটাকুটি করতাম বাইরের টুলস যেমন `cut`, `awk`, অথবা `sed` ব্যবহার করে। এখন, `string` আমাদের প্রধান বিল্ট-ইন ফাংশন যা ফিশ 2.3.0 তে চালু হয়। এটি দ্রুতগতির, আরও পাঠযোগ্য, এবং আমাদের স্ক্রিপ্টগুলির সাথে নির্বিঘ্নে একীভূত হয়েছে।

`string sub` আপনার একমাত্র বিকল্প নয়। অন্যান্য `string` ফাংশনগুলি স্ট্রিং ভাগ করতে, অংশ প্রতিস্থাপন করতে, অথবা তাদের যোগ করতে পারে। এই ফোকাস কম রিসোর্সের ব্যবহার এবং বোঝার সহজতার উপর।

বাস্তবায়ন বিষয়ে, যখন আপনি সাবস্ট্রিং এক্সট্র্যাক্ট করেন, ফিশ স্ট্রিংটি পড়ে এবং ঠিক যে অংশটি আপনি নির্দিষ্ট করেছেন শুধু মাত্র তা আউটপুট করে, ক্যারেক্টার এনকোডিংকে সম্মান করে এবং সাবস্ট্রিং এক্সট্র্যাকশনে সাধারণ বাগগুলি এড়িয়ে চলে যেমন একটি ক্যারেক্টারকে অর্ধেকে ভাগ করা।

## আরও দেখুন
- `string` সম্পর্কে অফিসিয়াল ফিশ ডকুমেন্টেশন: https://fishshell.com/docs/current/cmds/string.html
- ফিশ স্ক্রিপ্টিং সম্পর্কে কমিউনিটি টিউটোরিয়াল: https://fishshell.com/docs/current/tutorial.html
- ফিশ স্ট্রিং ম্যানিপুলেশন বিষয়ে স্ট্যাক ওভারফ্লো আলোচনা: https://stackoverflow.com/questions/tagged/fish
