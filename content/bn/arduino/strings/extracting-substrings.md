---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:43.110960-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0986\u09B0\u09A1\u09C1\u0987\
  \u09A8\u09CB \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u0995\
  \u09C7 `substring()` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u0995\u09BE\u099F\u09BE \u0993 \u09AB\u09BE\u09B2\u09BE \u0995\u09B0\u09BE \u09AF\
  \u09BE\u09AF\u09BC."
lastmod: '2024-03-17T18:47:44.311311-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B0\u09A1\u09C1\u0987\u09A8\u09CB \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982\u0997\u09C1\u09B2\u09BF\u0995\u09C7 `substring()` \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0995\u09BE\u099F\u09BE \u0993 \u09AB\
  \u09BE\u09B2\u09BE \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC."
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কিভাবে:
আরডুইনো স্ট্রিংগুলিকে `substring()` ব্যবহার করে কাটা ও ফালা করা যায়:

```arduino
void setup() {
  Serial.begin(9600);
  String phrase = "Hello, Arduino World!";
  String greeting = phrase.substring(0, 5);
  String location = phrase.substring(7, 19);
  
  Serial.println(greeting); // প্রিন্ট করে "Hello"
  Serial.println(location); // প্রিন্ট করে "Arduino World"
}

void loop() {
  // এখানে লুপ করার কিছু নেই।
}
```

সিরিয়াল মনিটরে আউটপুট:
```
Hello
Arduino World
```

## গভীর ডাইভ
আরডুইনো এটিকে সহজ করার আগে, প্রোগ্রামাররা চার অ্যারে এবং সি তে `strncpy` এর মতো ফাংশন ব্যবহার করত। কেবল ঐতিহাসিক নয়, তারা এখনও নিম্নস্তরের অপারেশনের জন্য ব্যবহার করা হয়। আরডুইনোতে `substring()` ফাংশন আসলে স্ট্রিং অবজেক্টস নিয়ে কাজ করা সহজ করে দেয়ার একটি উপায়। কিন্তু মনে রাখবেন, `String` ব্যবহার করলে মেমরি ফ্র্যাগমেন্টেশন হতে পারে। যদি স্থিতিশীলতা গুরুত্বপূর্ণ হয়, বিশেষ করে দীর্ঘকালীন অথবা জটিল প্রোগ্রামের ক্ষেত্রে, চার অ্যারের পুরানো পদ্ধতি বিবেচনা করুন।

`substring()` এর বিকল্প হল ডাইরেক্ট চার অ্যারে ম্যানিপুলেশন অথবা `strtok()` এর মতো ফাংশন। এগুলি আরও কার্যকর হতে পারে কিন্তু আপনাকে আরও বেশি কোড ম্যানেজ করতে হতে পারে।

অন্তরালে, `substring()` শুরুর ইনডেক্স থেকে শেষ ইনডেক্সের ঠিক আগের চরিত্রগুলিকে ধারণ করে এমন একটি নতুন স্ট্রিং অবজেক্ট তৈরি করে, যা শেষ পর্যন্ত সবকিছু চাইলে বাদ দেওয়া যেতে পারে।

## দেখুন:
- আরডুইনো স্ট্রিং রেফারেন্স: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- আরডুইনোতে মেমরি ম্যানেজমেন্ট: https://learn.arduino.cc/programming/variables-and-data-types/memory-management
- তুলনা করার জন্য সি++ `std::string` substr মেথড: http://www.cplusplus.com/reference/string/string/substr/
