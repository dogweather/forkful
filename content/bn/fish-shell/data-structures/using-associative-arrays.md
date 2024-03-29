---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:24:49.052208-06:00
description: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\
  \u09CD\u09AF\u09BE\u09B0\u09C7 \u09AC\u09BE \u09B9\u09CD\u09AF\u09BE\u09B6 \u09AE\
  \u09CD\u09AF\u09BE\u09AA \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09A1\u09C7\u099F\
  \u09BE \u0995\u09C0-\u09AE\u09BE\u09A8 \u099C\u09C1\u09A1\u09BC\u09BF \u09B9\u09BF\
  \u09B8\u09BE\u09AC\u09C7 \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3 \u0995\u09B0\
  \u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC, \u09AF\u09BE \u0995\u09C0 \u0985\u09A8\u09C1\
  \u09B8\u09BE\u09B0\u09C7 \u09A4\u09A5\u09CD\u09AF \u0986\u09AF\u09BC\u09CB\u099C\
  \u09A8 \u098F\u09AC\u0982 \u09AA\u09C1\u09A8\u09B0\u09C1\u09A6\u09CD\u09A7\u09BE\
  \u09B0 \u0995\u09B0\u09BE \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A6\u09C7\u09AF\
  \u09BC\u0964 \u09AF\u0996\u09A8 \u09B6\u09C1\u09A7\u09C1\u2026"
lastmod: '2024-03-17T18:47:44.488307-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\
  \u09CD\u09AF\u09BE\u09B0\u09C7 \u09AC\u09BE \u09B9\u09CD\u09AF\u09BE\u09B6 \u09AE\
  \u09CD\u09AF\u09BE\u09AA \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09A1\u09C7\u099F\
  \u09BE \u0995\u09C0-\u09AE\u09BE\u09A8 \u099C\u09C1\u09A1\u09BC\u09BF \u09B9\u09BF\
  \u09B8\u09BE\u09AC\u09C7 \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3 \u0995\u09B0\
  \u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC, \u09AF\u09BE \u0995\u09C0 \u0985\u09A8\u09C1\
  \u09B8\u09BE\u09B0\u09C7 \u09A4\u09A5\u09CD\u09AF \u0986\u09AF\u09BC\u09CB\u099C\
  \u09A8 \u098F\u09AC\u0982 \u09AA\u09C1\u09A8\u09B0\u09C1\u09A6\u09CD\u09A7\u09BE\
  \u09B0 \u0995\u09B0\u09BE \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A6\u09C7\u09AF\
  \u09BC\u0964 \u09AF\u0996\u09A8 \u09B6\u09C1\u09A7\u09C1\u2026"
title: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\u09CD\
  \u09AF\u09BE\u09B0\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0"
---

{{< edit_this_page >}}

## কি এবং কেন?

এসোসিয়েটিভ অ্যারে বা হ্যাশ ম্যাপ আপনাকে ডেটা কী-মান জুড়ি হিসাবে সংরক্ষণ করতে দেয়, যা কী অনুসারে তথ্য আয়োজন এবং পুনরুদ্ধার করা সহজ করে দেয়। যখন শুধু লিস্টের চেয়ে আরও গঠিত উপায়ে ডেটা সামাল দেওয়ার প্রয়োজন হয়, বিশেষ করে কনফিগারেশন এবং বিভিন্ন আট্রিবিউটের সাথে ডিল করার সময়, তখন এগুলি অত্যন্ত উপযোগী হয়।

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
