---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:36.792621-06:00
description: "\u0986\u09B0\u09A1\u09C1\u0987\u09A8\u09CB \u09AA\u09CD\u09B0\u0995\u09B2\
  \u09CD\u09AA\u09C7 \u09B0\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09AE \u09A8\u09AE\
  \u09CD\u09AC\u09B0 \u0989\u09CE\u09AA\u09BE\u09A6\u09A8 \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u098F\u09AE\u09A8 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A4\
  \u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AF\u09C7\u0997\u09C1\u09B2\u09BF \u09A1\
  \u09BF\u099C\u09BE\u0987\u09A8\u09C7\u09B0 \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE\
  \ \u0985\u09A8\u09C1\u09AE\u09BE\u09A8 \u0995\u09B0\u09BE \u09AF\u09BE\u09DF \u09A8\
  \u09BE\u0964 \u098F\u099F\u09BF \u0997\u09C7\u09AE\u09B8, \u09B8\u09BF\u09AE\u09C1\
  \u09B2\u09C7\u09B6\u09A8 \u098F\u09AC\u0982 \u09A8\u09BF\u09B0\u09BE\u09AA\u09A4\
  \u09CD\u09A4\u09BE \u09AA\u09CD\u09B0\u09A3\u09BE\u09B2\u09C0\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.318369-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B0\u09A1\u09C1\u0987\u09A8\u09CB \u09AA\u09CD\u09B0\u0995\u09B2\
  \u09CD\u09AA\u09C7 \u09B0\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09AE \u09A8\u09AE\
  \u09CD\u09AC\u09B0 \u0989\u09CE\u09AA\u09BE\u09A6\u09A8 \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u098F\u09AE\u09A8 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A4\
  \u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AF\u09C7\u0997\u09C1\u09B2\u09BF \u09A1\
  \u09BF\u099C\u09BE\u0987\u09A8\u09C7\u09B0 \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE\
  \ \u0985\u09A8\u09C1\u09AE\u09BE\u09A8 \u0995\u09B0\u09BE \u09AF\u09BE\u09DF \u09A8\
  \u09BE\u0964 \u098F\u099F\u09BF \u0997\u09C7\u09AE\u09B8, \u09B8\u09BF\u09AE\u09C1\
  \u09B2\u09C7\u09B6\u09A8 \u098F\u09AC\u0982 \u09A8\u09BF\u09B0\u09BE\u09AA\u09A4\
  \u09CD\u09A4\u09BE \u09AA\u09CD\u09B0\u09A3\u09BE\u09B2\u09C0\u09B0\u2026"
title: "\u098F\u09B2\u09CB\u09AE\u09C7\u09B2\u09CB \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
আরডুইনো প্রকল্পে র্যান্ডম নম্বর উৎপাদন করা মানে এমন সংখ্যা তৈরি করা যেগুলি ডিজাইনের দ্বারা অনুমান করা যায় না। এটি গেমস, সিমুলেশন এবং নিরাপত্তা প্রণালীর মতো অ্যাপ্লিকেশনগুলিতে অত্যাবশ্যক। প্রোগ্রামাররা এই কৌশলটি ব্যবহার করে পরিবর্তনশীলতা প্রবর্তন করে অথবা এমন সিদ্ধান্ত নেয় যা নিশ্চিতভাবে স্থির হওয়া উচিত নয়।

## কিভাবে:
আরডুইনো র্যান্ডম নম্বর উৎপাদনের জন্য সরল ফাংশনগুলি প্রদান করে: `randomSeed()` এবং `random()`. শুরু করতে, প্রোগ্রাম প্রতিবার চালু হলে বিভিন্ন ধারাবাহিক নম্বরের নিশ্চয়তা দিতে র্যান্ডম নম্বর জেনারেটরকে সিড করুন। একটি প্রায়শই ব্যবহৃত পদ্ধতি হল একটি অসংযুক্ত পিন থেকে একটি অ্যানালগ রিড দিয়ে সিড করা।

```Arduino
void setup() {
  Serial.begin(9600);
  // র্যান্ডম সিড ইনিশিয়ালাইজ
  randomSeed(analogRead(0));
}

void loop() {
  // র্যান্ডম নম্বর উৎপাদন করুন 0 থেকে 99 এর মধ্যে
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // আউটপুটের পাঠযোগ্যতা জন্য এক সেকেন্ড দেরি করুন
}
```

উপরের প্রোগ্রামটি `setup()` ফাংশনে র্যান্ডম নম্বর জেনারেটরটিকে ইনিশিয়ালাইজ করে এবং প্রতিটি লুপ ইটারেশনে 0 থেকে 99 এর মধ্যে একটি নতুন নম্বর উৎপাদন করে, সেই নম্বরটি সিরিয়াল মনিটরে আউটপুট করে।

নমুনা আউটপুট:
```
42
17
93
...
```

## গভীরে ডুব দিন
আরডুইনোর `random()` ফাংশন আসলে একটি প্যারো-র্যান্ডম নম্বর জেনারেটর (PRNG)-এর কাজ করে, যা একটি নির্দিষ্ট ধারাবাহিকতা অনুসরণ করে কিন্তু পরিসংখ্যানগত ভাবে র্যান্ডম বলে মনে হয়। ধারাবাহিকতার প্রাথমিক মান, অর্থাৎ সিড, এর অপ্রত্যাশিততার উপর গভীরভাবে প্রভাব ফেলে, তাই কিছুটা র্যান্ডম ইনপুটের সাথে `randomSeed()` এর ব্যবহার একটি শুরুর পয়েন্ট হিসাবে প্রচলিত। মনে রাখা জরুরি যে, আরডুইনো দ্বারা উৎপন্ন র্যান্ডমনেস বেশিরভাগ হবিস্ট প্রকল্পের জন্য যথেষ্ট হলেও, এর সময়ের সাথে পূর্বাভাসযোগ্যতা কারণে উচ্চ-নিরাপত্তার অ্যাপ্লিকেশনের জন্য মানদণ্ড পূরণ করে না। ক্রিপ্টোগ্রাফিক উদ্দেশ্যে, ফিজিক্যাল প্রক্রিয়াগুলিকে ব্যবহার করে সত্যিকারের র্যান্ডমনেস প্রদান করতে পারে এমন আরো জটিল অ্যালগরিদম এবং হার্ডওয়্যার র্যান্ডম নম্বর জেনারেটর (HRNGs)-এ দেখা উচিত।