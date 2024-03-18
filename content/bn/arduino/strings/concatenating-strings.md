---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:27.563136-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09DC\u09BE\
  \ \u09A6\u09C7\u0993\u09DF\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A6\u09C1\u099F\u09BF\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B6\u09C7\u09B7-\u09A5\
  \u09C7\u0995\u09C7-\u09B6\u09C7\u09B7 \u098F\u0995\u09A4\u09CD\u09B0\u09BF\u09A4\
  \ \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09A8\u09A4\u09C1\u09A8 \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE \u09AE\u09BF\u09B2\u09BE\u09A8\u09CB, \u0995\
  \u09AE\u09BE\u09A8\u09CD\u09A1 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u0985\
  \u09A5\u09AC\u09BE \u09B6\u09C1\u09A7\u09C1\u09AE\u09BE\u09A4\u09CD\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.314347-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09DC\u09BE \u09A6\
  \u09C7\u0993\u09DF\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A6\u09C1\u099F\u09BF \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B6\u09C7\u09B7-\u09A5\u09C7\
  \u0995\u09C7-\u09B6\u09C7\u09B7 \u098F\u0995\u09A4\u09CD\u09B0\u09BF\u09A4 \u0995\
  \u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09A8\u09A4\u09C1\u09A8 \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AC\u09BE\
  \u09B0\u09CD\u09A4\u09BE \u09AE\u09BF\u09B2\u09BE\u09A8\u09CB, \u0995\u09AE\u09BE\
  \u09A8\u09CD\u09A1 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u0985\u09A5\u09AC\
  \u09BE \u09B6\u09C1\u09A7\u09C1\u09AE\u09BE\u09A4\u09CD\u09B0\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
স্ট্রিং জোড়া দেওয়া মানে দুটি স্ট্রিংকে শেষ-থেকে-শেষ একত্রিত করে একটি নতুন স্ট্রিং তৈরি করা। প্রোগ্রামাররা বার্তা মিলানো, কমান্ড তৈরি করা অথবা শুধুমাত্র তথ্য সুন্দরভাবে প্রদর্শনের জন্য এটি করে থাকেন।

## কিভাবে:
চলুন কিছু স্ট্রিং জোড়া দেই! সবকিছু setup এর মধ্যে কারণ আমরা শুধু একটি দ্রুত দেখা পেতে চাই—পুনরাবৃত্তি লুপের প্রয়োজন নেই।

```arduino
void setup() {
  // সিরিয়াল যোগাযোগ শুরু করুন
  Serial.begin(9600);

  // দুটি স্ট্রিং তৈরি করুন
  String greeting = "Hello, ";
  String name = "Arduino!";

  // এগুলোকে জোড়া দিন
  String combined = greeting + name;

  // ফলাফল প্রিন্ট করুন
  Serial.println(combined); 
}
void loop() {
  // এখানে লুপ করার মত কিছু নেই
}
```

আপনি এটি চালান, এবং আউটপুট সিরিয়াল মনিটরে আপনার জন্য অপেক্ষা করে:

```
Hello, Arduino!
```

## গভীর ডুব
প্রোগ্রামিংয়ে স্ট্রিং জোড়া দেওয়া পুরনো পদ্ধতি—যখন প্রাথমিক ভাষাগুলো তাদের প্রথম পদক্ষেপ নিতে শুরু করে। আরডুইনোতে, আপনি আমাদের মত করে `+` অপারেটর ব্যবহার করতে পারেন, অথবা একটি স্ট্রিংকে আগে থেকে বিদ্যমান একটিতে যোগ করার জন্য `+=` ব্যবহার করতে পারেন। পেছনের দৃশ্যে, এই অপারেটরগুলি আসলে ফাংশনগুলি কল করে যা মেমোরি বরাদ্দ এবং অক্ষরগুলো দক্ষতার সাথে কপি করার কাজটি সামলায়।

সবসময় স্ট্রিং জোড়া দেওয়ার প্রয়োজন কেন নেই? ভাল, আপনি যদি ছোট মাইক্রোকন্ট্রোলার নিয়ে কাজ করেন এবং অনেক স্ট্রিং-এর মের্জিং করেন, আপনি মেমোরি সমস্যায় পড়তে পারেন—কারণ প্রতিবার আপনি যোগ করলে, আপনি একটি নতুন স্ট্রিং তৈরি করেন, যা আরো বেশি মেমোরি ব্যবহার করে। ভারি স্ট্রিং ম্যানিপুলেশনের জন্য, মানুষ কখনও কখনও চরিত্রের অ্যারেগুলি (ক্লাসিক C-স্টাইল) ব্যবহার করে যা স্থান বাঁচায় এবং সম্ভাব্য পারফরম্যান্সের সমস্যা এড়াতে সহায়তা করে।

আরও দেখুন, `concat()` এর মতো স্ট্রিং ফাংশনগুলি, যা শুধু স্ট্রিং নয়, অন্যান্য ডাটা টাইপগুলিকেও একটি বিদ্যমান স্ট্রিংয়ে যোগ করতে পারে।

## আরও দেখুন
আরও জানতে চান? এখানে গভীরে ডুব দিন:
- আরডুইনো স্ট্রিং রেফারেন্স: [arduino.cc/reference/en/language/variables/data-types/string/](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- আরডুইনোতে মেমোরি ম্যানেজমেন্ট: [learn.adafruit.com/memories-of-an-arduino](https://learn.adafruit.com/memories-of-an-arduino)
- আরডুইনো স্ট্রিংগুলির অসুবিধা: [majenko.co.uk/blog/evils-arduino-strings](https://majenko.co.uk/blog/evils-arduino-strings)
