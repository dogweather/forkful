---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:51.519204-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0986\u09B0\u09A1\u09C1\u0987\
  \u09A8\u09CB\u09B0 `String` \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u09C7\u09B0\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09C1\u09AC\u09BF\u09A7\u09BE\u099C\u09A8\u0995\
  \ `toLowerCase()` \u09AE\u09C7\u09A5\u09A1 \u0986\u099B\u09C7\u0964 \u0986\u09AA\
  \u09A8\u09BE\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\u09BC\u09C7\u09B0\
  \ \u0989\u09AA\u09B0 \u098F\u099F\u09BF \u0995\u09B2 \u0995\u09B0\u09C1\u09A8, \u098F\
  \u09AC\u0982 \u09A0\u09BF\u0995 \u09A4\u09C7\u09AE\u09A8\u09BF, \u098F\u099F\u09BF\
  \ \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0\u0995\u09C7\u09B8\u09C7 \u09AA\u09B0\u09BF\
  \u09A3\u09A4 \u09B9\u09AF\u09BC\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.309231-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B0\u09A1\u09C1\u0987\u09A8\u09CB\u09B0 `String` \u0985\u09AC\u099C\
  \u09C7\u0995\u09CD\u099F\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09B8\u09C1\u09AC\
  \u09BF\u09A7\u09BE\u099C\u09A8\u0995 `toLowerCase()` \u09AE\u09C7\u09A5\u09A1 \u0986\
  \u099B\u09C7\u0964 \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982\u09AF\u09BC\u09C7\u09B0 \u0989\u09AA\u09B0 \u098F\u099F\u09BF \u0995\
  \u09B2 \u0995\u09B0\u09C1\u09A8, \u098F\u09AC\u0982 \u09A0\u09BF\u0995 \u09A4\u09C7\
  \u09AE\u09A8\u09BF, \u098F\u099F\u09BF \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0\u0995\
  \u09C7\u09B8\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u09B9\u09AF\u09BC\u09C7 \u09AF\
  \u09BE\u09AC\u09C7\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কিভাবে:
আরডুইনোর `String` অবজেক্টের একটি সুবিধাজনক `toLowerCase()` মেথড আছে। আপনার স্ট্রিংয়ের উপর এটি কল করুন, এবং ঠিক তেমনি, এটি লোয়ারকেসে পরিণত হয়ে যাবে।

```Arduino
void setup() {
  Serial.begin(9600);
  String message = "Hello, World!";
  message.toLowerCase();
  Serial.println(message);  // আউটপুট: hello, world!
}

void loop() {
  // এখানে কিছু করার নেই।
}
```
আপনার সিরিয়াল মনিটর চালু করুন, এবং আপনি "hello, world!" প্রিন্ট হতে দেখবেন।

## গভীর ডুব
ঐতিহাসিকভাবে, টেক্সট নিয়ে কাজ করা প্রায়শই আপার এবং লোয়ার কেসের হিসেব রাখা জড়িত। ডেটা এন্ট্রি, সার্চ, এবং সর্টিং অপারেশনগুলি সাধারণত ব্যবহারকারীর ভুল কমাতে এবং শক্তিশালীতা বৃদ্ধি করতে কেস উপেক্ষা করে। অন্যান্য ভাষায়, যেমন সি, আপনি প্রতিটি অক্ষরের উপর ইটারেট করতেন এবং তাদেরকে পৃথকভাবে স্ট্যান্ডার্ড লাইব্রেরি ফাংশন ব্যবহার করে পরিণত করতেন। আরডুইনোতে, `String` অবজেক্টগুলি এই কার্যকারিতাকে সহজ ব্যবহারের জন্য মোড়ানো আছে।

বিকল্প? অবশ্যই। আপনি একটি `char` অ্যারে জন্য `toLowerCase()` ব্যবহার করতে পারেন, কিন্তু আপনাকে `<ctype.h>` থেকে `tolower()` সহ প্রতিটি অক্ষর দিয়ে হাঁটতে হবে এবং এটিকে পরিণত করতে হবে। যদি আপনি মেমরি এবং পারফরমেন্স নিয়ে চিন্তিত থাকেন, তাহলে `String` অবজেক্টের পরিবর্তে অক্ষরের অ্যারে ব্যবহার করুন এবং আপনার নিজস্ব লোয়ারকেসিং যুক্তি নিয়ে নিয়ন্ত্রণ নিন।

## আরো দেখুন
- আরডুইনোর `String` রেফারেন্স পেজ: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- অক্ষর অপারেশনের জন্য সি++ `<cctype>` লাইব্রেরি: http://www.cplusplus.com/reference/cctype/
- স্ট্রিং তুলনার কাজ কিভাবে হয় এবং কেস উপেক্ষা করা কেন গুরুত্বপূর্ণ তা বোঝার জন্য, দেখুন: https://en.wikipedia.org/wiki/String_(computer_science)#Comparison
