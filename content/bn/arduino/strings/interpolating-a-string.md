---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:10.483753-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u09B9\u099A\u09CD\u099B\u09C7 \u099F\
  \u09C7\u0995\u09CD\u09B8\u099F\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AD\u09C7\
  \u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2 \u09AE\u09BF\u09B6\u09CD\u09B0\u09BF\u09A4\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u09AE\u09C1\u09B9\u09C2\u09B0\u09CD\u09A4\u09C7 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\
  \u0995\u09C7\u09A8, \u09AF\u09BE \u0986\u0989\u099F\u09AA\u09C1\u099F\u0995\u09C7\
  \ \u0997\u09A4\u09BF\u09B6\u09C0\u09B2 \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.308209-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u09B9\u099A\u09CD\u099B\u09C7 \u099F\
  \u09C7\u0995\u09CD\u09B8\u099F\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AD\u09C7\
  \u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2 \u09AE\u09BF\u09B6\u09CD\u09B0\u09BF\u09A4\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u09AE\u09C1\u09B9\u09C2\u09B0\u09CD\u09A4\u09C7 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\
  \u0995\u09C7\u09A8, \u09AF\u09BE \u0986\u0989\u099F\u09AA\u09C1\u099F\u0995\u09C7\
  \ \u0997\u09A4\u09BF\u09B6\u09C0\u09B2 \u098F\u09AC\u0982 \u09AA\u09BE\u09A0\u09AF\
  \u09CB\u0997\u09CD\u09AF \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কি এবং কেন?

স্ট্রিং ইন্টারপোলেশন হচ্ছে টেক্সটের সাথে ভেরিয়েবল মিশ্রিত করা। প্রোগ্রামাররা মুহূর্তে স্ট্রিং তৈরি করার জন্য এটি করে থাকেন, যা আউটপুটকে গতিশীল এবং পাঠযোগ্য করে তোলে।

## কিভাবে:

Arduino-তে বিল্ট-ইন স্ট্রিং ইন্টারপোলেশন নেই, তবে `sprintf()` ফাংশন ব্যবহার করে অথবা স্ট্রিং এবং ভেরিয়েবল যুক্ত করে অনুরূপ ফলাফল পাওয়া যেতে পারে।

```Arduino
char buffer[50]; // নিশ্চিত করুন এটি চূড়ান্ত স্ট্রিং ধারণ করার জন্য যথেষ্ট বড়
int sensorValue = analogRead(A0);
sprintf(buffer, "Sensor reading: %d", sensorValue);
Serial.println(buffer);
```

আউটপুট:
```
Sensor reading: 402
```

অথবা স্ট্রিং যুক্ত করে:

```Arduino
String message = "Sensor reading: " + String(sensorValue);
Serial.println(message);
```

## গভীর ডুব

C এবং C++ (Arduino স্কেচের মূল ভাষা) ঐতিহ্যগতভাবে নতুন ভাষা (যেমন, Python বা JavaScript) এর মত স্ট্রিং ইন্টারপোলেশন সাপোর্ট করে না। পরিবর্তে, `sprintf()` ভেরিয়েবলের সাথে স্ট্রিং গঠনের জন্য প্রধান পদ্ধতি হয়ে উঠেছে। এটি কাজ করে, কিন্তু সাবধানে ম্যানেজ না করা হলে বাফার ওভারফ্লোর কারণে এটি কিছুটা বিড়ম্বনাপূর্ণ এবং ত্রুটিপূর্ণ হতে পারে।

`String` ক্লাসের সাহায্যে যুক্ত করা আরও সহজবোধ্য এবং মেমোরি ত্রুটি থেকে নিরাপদ। ড্রয়ব্যাক? এটি মেমোরি বিচ্ছিন্নতার দিকে নিয়ে যেতে পারে, বিশেষ করে মেমোরি-সীমাবদ্ধ ডিভাইসের যেমন Arduino-তে দীর্ঘমেয়াদী প্রোগ্রামে।

কিছু নতুন বা বিশেষায়িত C++ লাইব্রেরিতে (Arduino-তে মানক নয়) ইন্টারপোলেশনের আরও কাছের সিনট্যাক্স প্রদানকারী স্ট্রিং ফরম্যাটিং লাইব্রেরি যেমন `fmtlib` ব্যবহার করার বিকল্প পাওয়া যায়।

বাস্তবায়ন বিস্তারিত প্রসঙ্গে, `String` ক্লাসের সাথে যুক্ত করার সময়, আবাসিকভাবে, Arduino নতুন স্ট্রিং অবজেক্ট তৈরি করে এবং আপনার জন্য মেমোরি ম্যানেজ করে। অন্যদিকে, `sprintf()` একটি বাফারে ফর্ম্যাটেড টেক্সট লিখে, যা আপনাকে ম্যানুয়ালি মেমোরি ম্যানেজ করতে বাধ্য করে, ফলস্বরূপ আরও নিয়ন্ত্রণের সুযোগ দেয়।

## দেখুন ও

- Arduino `String` ক্লাস রেফারেন্স: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- `sprintf()` ফাংশন রেফারেন্স: http://www.cplusplus.com/reference/cstdio/sprintf/
- Arduino মেমোরি অপ্টিমাইজেশন: https://www.arduino.cc/en/Tutorial/Foundations/Memory
- fmtlib, একটি আধুনিক স্ট্রিং ফর্ম্যাটিং লাইব্রেরি: https://fmt.dev/latest/index.html
