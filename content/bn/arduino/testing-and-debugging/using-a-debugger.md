---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:21:37.641264-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Arduino IDE \u098F\u09B0 \u09B8\
  \u09BE\u09A5\u09C7, \u0986\u09AA\u09A8\u09BF Serial \u09AA\u09CD\u09B0\u09BF\u09A8\
  \u09CD\u099F\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u09A1\u09BF\u09AC\u09BE\u0997 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\
  \u09A8, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u098F\u099F\u09BF \u098F\u0995\u099F\
  \u09BF \u0997\u09C1\u09B9\u09BE \u0985\u09A8\u09CD\u09AC\u09C7\u09B7\u09A3\u09C7\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u099F\u09B0\u09CD\u099A\u09B2\u09BE\u0987\u099F\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u09AE\u09A4\u09CB \u098F\
  \u0995\u099F\u09C1\u2026"
lastmod: '2024-03-17T18:47:44.326409-06:00'
model: gpt-4-0125-preview
summary: "Arduino IDE \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7, \u0986\u09AA\u09A8\u09BF\
  \ Serial \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F\u09B8 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A1\u09BF\u09AC\u09BE\u0997 \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1\
  \ \u098F\u099F\u09BF \u098F\u0995\u099F\u09BF \u0997\u09C1\u09B9\u09BE \u0985\u09A8\
  \u09CD\u09AC\u09C7\u09B7\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u099F\u09B0\
  \u09CD\u099A\u09B2\u09BE\u0987\u099F \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\
  \u09B0 \u09AE\u09A4\u09CB \u098F\u0995\u099F\u09C1 \u0985\u09CD\u09AF\u09BE\u09B2\
  \u09CB\u09AE\u09C7\u09B2\u09CB\u0964 \u0986\u09B8\u09B2 \u09A1\u09BF\u09AC\u09BE\
  \u0997\u09BF\u0982 \u098F\u09B0 \u099C\u09A8\u09CD\u09AF, \u0986\u09AA\u09A8\u09BF\
  \ \u09B9\u09AF\u09BC\u09A4\u09CB \u0986\u09AA\u09A8\u09BE\u09B0 \u0996\u09C7\u09B2\
  \u09BE \u0986\u09B0\u0993 \u09AC\u09BE\u09A1\u09BC\u09BF\u09AF\u09BC\u09C7 \u09A6\
  \u09BF\u09A4\u09C7 \u099A\u09BE\u0987\u09AC\u09C7\u09A8 \u09AF\u09C7\u09AE\u09A8\
  \ \u098F\u0995\u099F\u09BF Atmel-ICE \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0\
  \ \u09AF\u09BE Arduino \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\u09C7\u09B0 \u09B8\u09BE\
  \u09A5\u09C7 \u0987\u09A8\u09CD\u099F\u09BF\u0997\u09CD\u09B0\u09C7\u099F \u0995\
  \u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 Serial \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC-\u09A1\
  \u09BF\u09AC\u09BE\u0997\u09BF\u0982 \u098F\u09B0 \u098F\u0995\u099F\u09BF \u09B8\
  \u09CD\u09AC\u09BE\u09A6 \u09AA\u09BE\u09AC\u09C7\u09A8."
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
weight: 35
---

## কিভাবে:
Arduino IDE এর সাথে, আপনি Serial প্রিন্টস ব্যবহার করে ডিবাগ করতে পারেন, কিন্তু এটি একটি গুহা অন্বেষণের জন্য টর্চলাইট ব্যবহারের মতো একটু অ্যালোমেলো। আসল ডিবাগিং এর জন্য, আপনি হয়তো আপনার খেলা আরও বাড়িয়ে দিতে চাইবেন যেমন একটি Atmel-ICE ডিবাগার যা Arduino পরিবেশের সাথে ইন্টিগ্রেট করে। এখানে Serial ব্যবহার করে প্রায়-ডিবাগিং এর একটি স্বাদ পাবেন:

```Arduino
void setup() {
  Serial.begin(9600);
}
void loop() {
  int sensorValue = analogRead(A0);
  Serial.print("Sensor Value: ");
  Serial.println(sensorValue);
  // কল্পনা করুন আপনি এখানে 512 আশা করছেন, কিন্তু 0 পাচ্ছেন।
  // সেন্সর সংযোগ পরীক্ষা করার সময় 
  delay(1000); // আবার পড়ার আগে এক সেকেন্ড অপেক্ষা করুন
}
```
Serial মনিটর খোলা অবস্থায় এটি চালান, এবং আপনি আপনার সেন্সর কী নির্গত করছে তা বাস্তব সময়ে দেখবেন।

## গভীর ডুব
ডিবাগারগুলির আগে, এটি একটি প্রিন্ট বিবৃতির দুনিয়া ছিল - সবকিছু প্রিন্ট করে আপনি কেবল অনুমান করতে পারতেন যে কী হচ্ছে। Arduino এর মতো সহজ পরিবেশ বা সীমাবদ্ধ হার্ডওয়্যারে প্রিন্ট দিয়ে ডিবাগিং এখনও সাধারণ হয়ে উঠেছে।

Atmel-ICE এর মতো সার্কিটের ভেতরের এমুলেটরগুলির বিকল্পগুলি অন্তর্ভুক্ত `avr-gdb` এর মতো সফটওয়্যার ডিবাগিং টুলগুলি। আপনি এটি `avarice` এর সাথে জোড়া দিতে পারেন যাতে GDB এবং আপনার হার্ডওয়্যারের মধ্যে একটি সেতু তৈরি হয়, যা চিপের উপর আরও উন্নত ডিবাগিংয়ের জন

একটি ডিবাগার ব্যবহার করে, আপনি নির্দিষ্ট পয়েন্টে নির্বাহ থামানোর জন্য ব্রেকপয়েন্ট সেট করতে পারেন। আপনি আপনার কোড লাইন ধরে লাইন পরীক্ষা করতে পারেন, মেমোরি, রেজিস্টার, এবং ভেরিয়েবল পরীক্ষা করতে পারেন। এটি আপনাকে অন্ধকারে অন্ধকারে শট নেওয়ার পরিবর্তে সমস্যাগুলির সঠিক অবস্থান চিহ্নিত করতে দেয়। একটি ডিবাগার বাস্তবায়ন করার সময়, নিশ্চিত করুন আপনার পরিবেশটি সঠিকভাবে সেটআপ করা আছে - মিল না খাওয়া সংস্করণ বা খারাপভাবে কনফিগার করা টুলগুলি ক্ষোভের কারণ হতে পারে।

## আরও দেখুন
গভীরে যেতে প্রস্তুত? এগুলোতে ডাইভ করুন:
- Arduino ডিবাগিং গাইড [Arduino Debugging](https://www.arduino.cc/en/Guide/Environment#toc7)
- avr-gdb সেটআপের জন্য AVR Libc রেফারেন্স ম্যানুয়াল: [AVR Libc হোম পেজ](http://www.nongnu.org/avr-libc/)
