---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:24:49.052208-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AB\u09BF\u09B6 \u09A8\u09C7\
  \u099F\u09BF\u09AD\u09AD\u09BE\u09AC\u09C7 Bash 4+ \u098F\u09B0 \u09AE\u09A4\u09CB\
  \ \u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\u09CD\
  \u09AF\u09BE\u09B0\u09C7 \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u0995\u09B0\u09C7\
  \ \u09A8\u09BE, \u09A4\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF \u09A4\u09BE\u09B2\u09BF\
  \u0995\u09BE \u098F\u09AC\u0982 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AE\
  \u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8\u09C7\u09B0 \u09B8\
  \u09AE\u09A8\u09CD\u09AC\u09AF\u09BC \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 \u0985\u09A8\u09C1\u09B0\u09C2\u09AA \u09AB\u09BE\u0982\u09B6\
  \u09A8\u09BE\u09B2\u09BF\u099F\u09BF\u2026"
lastmod: '2024-04-05T21:53:53.151285-06:00'
model: gpt-4-0125-preview
summary: "\u09AB\u09BF\u09B6 \u09A8\u09C7\u099F\u09BF\u09AD\u09AD\u09BE\u09AC\u09C7\
  \ Bash 4+ \u098F\u09B0 \u09AE\u09A4\u09CB \u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\
  \u09C7\u099F\u09BF\u09AD \u0985\u09CD\u09AF\u09BE\u09B0\u09C7 \u09B8\u09AE\u09B0\
  \u09CD\u09A5\u09A8 \u0995\u09B0\u09C7 \u09A8\u09BE, \u09A4\u09AC\u09C7 \u0986\u09AA\
  \u09A8\u09BF \u09A4\u09BE\u09B2\u09BF\u0995\u09BE \u098F\u09AC\u0982 \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\
  \u09C7\u09B6\u09A8\u09C7\u09B0 \u09B8\u09AE\u09A8\u09CD\u09AC\u09AF\u09BC \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0985\u09A8\u09C1\u09B0\
  \u09C2\u09AA \u09AB\u09BE\u0982\u09B6\u09A8\u09BE\u09B2\u09BF\u099F\u09BF \u0985\
  \u09B0\u09CD\u099C\u09A8 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0 \u0985\u09A8\
  \u09C1\u0995\u09B0\u09A3 \u0995\u09B0\u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0983 \u09AA\u09CD\u09B0\u09A5\
  \u09AE\u09C7, \"\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD\
  \ \u0985\u09CD\u09AF\u09BE\u09B0\u09C7\" \u0989\u09AA\u09BE\u09A6\u09BE\u09A8\u0997\
  \u09C1\u09B2\u09CB \u09AA\u09C3\u09A5\u0995\u09AD\u09BE\u09AC\u09C7 \u09B8\u09C7\
  \u099F\u0986\u09AA \u0995\u09B0\u09C1\u09A8."
title: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\u09CD\
  \u09AF\u09BE\u09B0\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0"
weight: 15
---

## কিভাবে:
ফিশ নেটিভভাবে Bash 4+ এর মতো এসোসিয়েটিভ অ্যারে সমর্থন করে না, তবে আপনি তালিকা এবং স্ট্রিং ম্যানিপুলেশনের সমন্বয় ব্যবহার করে অনুরূপ ফাংশনালিটি অর্জন করতে পারেন। এখানে তাদের অনুকরণ করার উপায় দেওয়া হলঃ

প্রথমে, "এসোসিয়েটিভ অ্যারে" উপাদানগুলো পৃথকভাবে সেটআপ করুন:

```Fish Shell
set food_color_apple "red"
set food_color_banana "yellow"
```

একটি উপাদানে একসেস করতে, শুধু তাকে সরাসরি রেফারেন্স করুন:

```Fish Shell
echo $food_color_apple
# আউটপুট: red
```

যদি আপনাকে তাদের উপরে ইটারেট করা প্রয়োজন হয়, নামকরণ কনভেনশন বিবেচনা করে একটি ফর-লুপ ব্যবহার করুন:

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# আউটপুট:
# red
# yellow
```

যারা Bash's `${!array[@]}` মিস করছেন সব কী পাওয়ার জন্য, আপনি কীগুলি একটি পৃথক তালিকায় সংরক্ষণ করতে পারেন:

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'is' $food_color_$key
end
# আউটপুট:
# apple is red
# banana is yellow
```

## ডীপ ডাইভ
অন্যান্য স্ক্রিপ্টিং ভাষায় যেমন সত্যিই এসোসিয়েটিভ অ্যারে রয়েছে, তেমনি ফিশের দৃষ্টিকোণের অংশ এখনো নয়। দেখানো ওয়ার্কঅ্যারাউন্ডটি ফিশের স্ট্রিং ম্যানিপুলেশন এবং তালিকার ক্ষমতাদী ব্যবহার করে একটি প্রতিকারী-এসোসিয়েটিভ অ্যারে কাঠামো তৈরি করে। এটি কাজ করে, তবে বিল্ট-ইন এসোসিয়েটিভ অ্যারে সাপোর্টের মতো পরিষ্কার বা ত্রুটিমুক্ত নয়। Bash এবং Zsh এর মতো অন্যান্য শেল বিল্ট-ইন এসোসিয়েটিভ অ্যারে ফাংশনালিটি প্রদান করে, যা আরও সরল, পঠনযোগ্য কোডে ফলাফল দেয়। যাইহোক, ফিশের ডিজাইন দর্শনের লক্ষ্য হল সহজতা এবং ব্যবহারকারী-বান্ধবতা, যা এই ধরনের বৈশিষ্ট্যগুলির খরচে হতে পারে। ওয়ার্কঅ্যারাউন্ডটি বেশিরভাগ প্রয়োজন মেটায় তবে ফিশ শেল-এর বিবর্তনের দিকে নজর রাখুন—এর ডেভেলপাররা সাম্প্রদায়িক প্রতিক্রিয়ার ভিত্তিতে নিয়মিত উন্নতি এবং বৈশিষ্ট্য যোগ করে চলেছেন।
