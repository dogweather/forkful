---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:07:00.620792-06:00
description: "\u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2 \u09AE\u09A8\u09BF\u099F\
  \u09B0\u09C7 \u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F\
  \ \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE \u09B9\u09B2 \u0986\
  \u09B0\u09A1\u09C1\u0987\u09A8\u09CB\u09B0 \u09AE\u09A8 \u09AD\u09C7\u09A6 \u0995\
  \u09B0\u09C7 \u09A6\u09C7\u0996\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u0989\u09AA\
  \u09BE\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\
  \u09A8 \u0997\u09CD\u09B0\u09C7\u09AE\u09B2\u09BF\u09A8\u09B8 \u099F\u09CD\u09B0\
  \u09CD\u09AF\u09BE\u0995 \u0995\u09B0\u09A4\u09C7, \u09A7\u09BE\u09B0\u09A3\u09BE\
  \ \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09A4\u09C7,\u2026"
lastmod: '2024-03-17T18:47:44.324403-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2 \u09AE\u09A8\u09BF\u099F\
  \u09B0\u09C7 \u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F\
  \ \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE \u09B9\u09B2 \u0986\
  \u09B0\u09A1\u09C1\u0987\u09A8\u09CB\u09B0 \u09AE\u09A8 \u09AD\u09C7\u09A6 \u0995\
  \u09B0\u09C7 \u09A6\u09C7\u0996\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u0989\u09AA\
  \u09BE\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\
  \u09A8 \u0997\u09CD\u09B0\u09C7\u09AE\u09B2\u09BF\u09A8\u09B8 \u099F\u09CD\u09B0\
  \u09CD\u09AF\u09BE\u0995 \u0995\u09B0\u09A4\u09C7, \u09A7\u09BE\u09B0\u09A3\u09BE\
  \ \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09A4\u09C7, \u098F\u09AC\
  \u0982 \u09AC\u09BF\u09B0\u09BE\u09AE\u09B9\u09C0\u09A8 \u09A1\u09BF\u09AC\u09BE\
  \u0997\u09BF\u0982 \u0995\u09CC\u09B6\u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u09A8\u09BE \u0995\u09B0\u09C7\u0987 \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC\
  \ \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u09A1\u09BE\u099F\u09BE \u09AE\u09A8\u09BF\
  \u099F\u09B0 \u0995\u09B0\u09A4\u09C7\u0964."
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কিভাবে:
চলুন ব্যাপারের মূল কথায় আসি। ধরুন আপনি চান "Hello, world!" প্রতি সেকেন্ডে প্রিন্ট করতে। এখানে কোড স্নিপেটটি:

```Arduino
void setup() {
  Serial.begin(9600);  // সিরিয়াল যোগাযোগ শুরু করুন
}

void loop() {
  Serial.println("Hello, world!");  // বার্তা প্রিন্ট করুন
  delay(1000);  // এক সেকেন্ড অপেক্ষা করুন
}
```

আরডুইনো IDE এর সিরিয়াল মনিটর চালু করুন এবং ঘড়ির মতো করে শব্দগুলো নেমে আসতে দেখুন। নমুনা আউটপুট:

```
Hello, world!
Hello, world!
Hello, world!
...
```

## গভীর ডাইভ
`Serial` আমাদের বিশ্বাসযোগ্য সহচর হয়ে ওঠার আগে, মানুষ যোগাযোগের জন্য মর্চের লাইট ঝলসানো ব্যবহার করত - ডিবাগিং এর স্টোন এজ। তারপর, গুরুত্বপূর্ণ ডিবাগ হার্ডওয়্যার এসেছিল, কিন্তু তা ছিল দামী। `Serial.print()` এবং এর আত্মীয়রা এখন আমাদের স্ক্রিনে পাঠ্য ছুঁড়ে দিতে দেয়, দ্রুত গতিতে, সস্তার মতো।

বিকল্প? আপনার কাছে এলসিডি, এসডি কার্ডে লগিং, এমনকি তারের প্রতি অনীহায় ব্লুটুথও রয়েছে। প্রতিটি পদ্ধতির নিজস্ব বৈশিষ্ট্য রয়েছে; `Serial` হল সরাসরি গোলাবারুদ - সাধারণ, সরাসরি, সর্বদা উপস্থিত।

আড়ালে, `Serial.print()` আপনার ডেটাকে বাইটে রূপান্তর করে যা ইউএসবির মাধ্যমে আপনার কম্পিউটারে ছুটে চলে। এটি হার্ডওয়্যার (UART) অথবা সফটওয়্যার-অনুকরণীয় (SoftSerial) সিরিয়াল পোর্টের মাধ্যমে ঘটে। এটি বিশ্বাসযোগ্য, কিন্তু খুব বেশি ডাটা দিয়ে পোর্টকে ব্যস্ত রাখা আপনার প্রোগ্রামের প্রবাহে জ্যাম তৈরি করতে পারে, তাই সিরিয়াল প্রিন্টস ছড়িয়ে দিন যেন আপনি একটি স্টেক মশলা দিচ্ছেন, স্যুপে বন্যা এনে দিচ্ছেন না।

## আরও দেখুন
আরও জানার জন্য:

- আরডুইনোর `Serial` গাইড: [Arduino Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- সিরিয়াল যোগাযোগের বিজ্ঞান সম্পর্কে: [UART Communication](https://www.sparkfun.com/tutorials/215)
