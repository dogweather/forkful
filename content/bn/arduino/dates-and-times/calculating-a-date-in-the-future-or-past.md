---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:53.333095-06:00
description: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\
  \u09C0\u09A4\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09A8\u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09A3 \u0995\u09B0\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u098F\u0995\
  \u099F\u09BF \u09A6\u09BF\u09A8 \u09A0\u09BF\u0995 \u0995\u09B0\u09BE, \u09AF\u09BE\
  \ \u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F\
  \ \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u0986\u0997\u09C7 \u09AC\u09BE \u09AA\
  \u09B0\u09C7 \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u0987 \u0995\u09BE\u099C\u099F\u09BF\
  \ \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u0998\u099F\u09A8\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.336094-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A8\
  \u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09A3 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u098F\u0995\u099F\
  \u09BF \u09A6\u09BF\u09A8 \u09A0\u09BF\u0995 \u0995\u09B0\u09BE, \u09AF\u09BE \u098F\
  \u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A4\
  \u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u0986\u0997\u09C7 \u09AC\u09BE \u09AA\u09B0\
  \u09C7 \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u0987 \u0995\u09BE\u099C\u099F\u09BF \u0995\
  \u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u0998\u099F\u09A8\u09BE\u2026"
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
ভবিষ্যত বা অতীতের একটি তারিখ নির্ধারণ করা মানে নির্দিষ্ট একটি দিন ঠিক করা, যা একটি নির্দিষ্ট তারিখের আগে বা পরে হয়। প্রোগ্রামাররা এই কাজটি করে থাকেন ঘটনা সমূহের সময়সূচি, অনুস্মারক বা মেয়াদের গণনা করার মতো কার্যক্রমের জন্য।

## কিভাবে:

Arduino-এ সরাসরি তারিখ ও সময় নিয়ে কাজ করার বিল্ট-ইন ফাংশন নেই, তবে আপনি "TimeLib.h" লাইব্রেরি ব্যবহার করে তারিখ নির্ণয়ের কাজ করতে পারেন। নিচের উদাহরণগুলো ব্যবহারের আগে নিশ্চিত করুন যে আপনি লাইব্রেরিটি ইনস্টল করেছেন।

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(10, 0, 0, 25, 3, 2023); // সময় সেট করুন মার্চ 25, 2023, 10:00:00 এ
}

void loop() {
  // ভবিষ্যতে 10 দিন গণনা করুন
  time_t futureTime = now() + 10 * SECS_PER_DAY;
  
  // ভবিষ্যতের তারিখ প্রিন্ট করুন
  Serial.print(day(futureTime));
  Serial.print("/");
  Serial.print(month(futureTime));
  Serial.print("/");
  Serial.println(year(futureTime));

  // অতীতে 10 দিন গণনা করুন
  time_t pastTime = now() - 10 * SECS_PER_DAY;
  
  // অতীতের তারিখ প্রিন্ট করুন
  Serial.print(day(pastTime));
  Serial.print("/");
  Serial.print(month(pastTime));
  Serial.print("/");
  Serial.println(year(pastTime));

  // ক্রমাগত প্রিন্টিং এড়ানোর জন্য
  delay(10000);
}
```
নমুনা আউটপুট:
```
4/4/2023
15/3/2023
```

## গভীর ডুব

RTC (রিয়েল-টাইম ঘড়ি) মডিউল এবং TimeLib এর মতো লাইব্রেরি ছাড়া, Arduino তে সময় রক্ষণাবেক্ষণ বিদ্যমান ছিল অতি প্রাথমিক এবং সাধারণত নিজে নিজে বাস্তবায়িত। ভবিষ্যত অথবা অতীতের তারিখ গণনা করার বিভিন্ন পদ্ধতি রয়েছে, তবে TimeLib এর মতো বিশেষায়িত লাইব্রেরি ব্যবহার করা প্রক্রিয়াটিকে অনেক সহজ করে তোলে।

TimeLib ছাড়া বিকল্প হিসেবে আছে "RTClib.h", যা হার্ডওয়্যার RTCs এর সাথে কাজ করতে আরো ব্যাপকভাবে ব্যবহৃত, অথবা সংক্ষিপ্ত সময় ব্যবধানের জন্য বিল্ট-ইন `millis()` ফাংশন (ম্যানুয়াল তারিখ ব্যবস্থাপনা সহ)। TimeLib অধিবর্ষ এবং সময় অঞ্চলগুলি সামাল দেয় এবং সহজে তারিখের ম্যানিপুলেশনের জন্য উপযোগী ফাংশন সরবরাহ করে।

ভবিষ্যত অথবা অতীতের তারিখ গণনা করতে গেলে, আপনি যদি বাস্তব সময়ের ঘড়ি অথবা বাহ্যিক সময়ের উৎসের সাথে কাজ করেন তবে সময় অঞ্চল এবং গ্রীষ্মকালীন সময় সঞ্চয়ের পরিবর্তনগুলির জন্য সতর্ক থাকুন। Arduino তে, RTC বা ইন্টারনেট সংযোগ ছাড়া, আপনি সাধারণত সময়টি ম্যানুয়ালি সেট করবেন অথবা বাহ্যিক সংকেত (যেমন GPS বা রেডিও সময় সংকেত) এর মাধ্যমে।

## দেখুন

- টাইম লাইব্রেরি ডকুমেন্টেশন:
  https://www.arduino.cc/reference/en/libraries/time/
- RTClib, বাস্তব সময়ের ঘড়ি নিয়ে কাজ করার জন্য জনপ্রিয় একটি লাইব্রেরি:
  https://github.com/adafruit/RTClib
- Arduino-র millis() ফাংশন এবং এর ব্যবহার:
  https://www.arduino.cc/reference/en/language/functions/time/millis/
