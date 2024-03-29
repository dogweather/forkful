---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:26.614294-06:00
description: "C \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC \u09AC\u09B0\u09CD\u09A4\u09AE\
  \u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 \u0997\u09C7\
  \u09B2\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\
  \u09A1 C \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09A4\u09C7 \u0986\
  \u0998\u09BE\u09A4 \u0995\u09B0\u09C7 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\
  \u09C7\u09B0 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A6\u09BF\u09A8 \u098F\
  \u09AC\u0982 \u09B8\u09AE\u09AF\u09BC \u0986\u09A8\u09BE\u09A8\u09CB \u098F\u09AC\
  \u0982 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u0995\u09B0\u09BE \u09AA\u09CD\
  \u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987\u2026"
lastmod: '2024-03-17T18:47:44.553541-06:00'
model: gpt-4-0125-preview
summary: "C \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\
  \u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 \u0997\u09C7\u09B2\
  \u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ C \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09A4\u09C7 \u0986\u0998\
  \u09BE\u09A4 \u0995\u09B0\u09C7 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\
  \u09B0 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A6\u09BF\u09A8 \u098F\u09AC\
  \u0982 \u09B8\u09AE\u09AF\u09BC \u0986\u09A8\u09BE\u09A8\u09CB \u098F\u09AC\u0982\
  \ \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u0995\u09B0\u09BE \u09AA\u09CD\u09B0\
  \u09AF\u09BC\u09CB\u099C\u09A8\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987\u2026"
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
---

{{< edit_this_page >}}

## কী এবং কেন?

C ভাষায় বর্তমান তারিখ পেতে গেলে স্ট্যান্ডার্ড C লাইব্রেরিতে আঘাত করে সিস্টেমের বর্তমান দিন এবং সময় আনানো এবং ফরম্যাট করা প্রয়োজন। প্রোগ্রামাররা প্রায়ই তাদের অ্যাপ্লিকেশনের মধ্যে লগিং, টাইম-স্ট্যাম্পিং, বা সিডিউলিং বৈশিষ্ট্যের জন্য এই কার্যকারিতা প্রয়োজন হয়।

## কিভাবে:

C তে, `<time.h>` হেডার তারিখ এবং সময় নিয়ে কাজ করার জন্য প্রয়োজনীয় ফাংশন এবং টাইপগুলি প্রদান করে। `time()` ফাংশন বর্তমান সময় নেয়, অন্যদিকে `localtime()` এই সময়টি স্থানীয় সময় অঞ্চলে রূপান্তর করে। তারিখ দেখানোর জন্য, আমরা এটি একটি স্ট্রিং হিসেবে ফরম্যাট করতে `strftime()` ব্যবহার করি।

এখানে একটি মৌলিক উদাহরণ:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char buffer[80];
    time_t rawtime;
    struct tm *timeinfo;

    // বর্তমান সময় নিন
    time(&rawtime);
    // এটিকে স্থানীয় সময়ে রূপান্তর করুন
    timeinfo = localtime(&rawtime);
    
    // তারিখ ফরম্যাট করে প্রিন্ট করুন
    strftime(buffer, 80, "আজকের তারিখ হলো %Y-%m-%d", timeinfo);
    printf("%s\n", buffer);

    return 0;
}
```

নমুনা আউটপুট এরকম দেখাবে:

```
আজকের তারিখ হলো 2023-04-12
```

## গভীর ডুব

C তে সময় পরিচালনা, যা `<time.h>` দ্বারা সম্ভব হয়, ভাষার ও UNIX সিস্টেমের প্রাথমিক দিনগুলির দিকে রেফার করে। এটি `time_t` ডেটা টাইপের চারপাশে নির্মিত, যা বর্তমান সময়কে ইউনিক্স এপোকের (জানুয়ারি ১, ১৯৭০) থেকে সেকেন্ডের সংখ্যা হিসেবে প্রকাশ করে। এটি কার্যকর এবং বিশ্বব্যাপী সামঞ্জস্যপূর্ণ হলেও, এটি এও মানে যে `time_t` এর পরিসর এবং রেজোলিউশন দ্বারা স্ট্যান্ডার্ড C লাইব্রেরির সময় ফাংশনগুলি স্বাভাবিকভাবে সীমাবদ্ধ।

বর্তমান অ্যাপ্লিকেশনগুলি, বিশেষ করে যেগুলি উচ্চ-রেজোলিউশন টাইমস্ট্যাম্প বা অনেক দূরের বা অতীতের তারিখ নিয়ে কাজ করে, এই সীমাবদ্ধতাগুলি চ্যালেঞ্জিং খুঁজে পেতে পারে। উদাহরণ স্বরূপ, বছর ২০৩৮ সমস্যা একটি বিখ্যাত উদাহরণ যেখানে ৩২-বিট `time_t` ব্যবহার করা সিস্টেম গুলি ওভারফ্লো হবে।

আরও জটিল সময় এবং তারিখ পরিচালনার জন্য, অনেক প্রোগ্রামার বাইরের লাইব্রেরিগুলি বা অপারেটিং সিস্টেম দ্বারা প্রদত্ত কার্যকারিতাগুলির দিকে মুখ করে। যেমন, C++ এ, `<chrono>` লাইব্রেরি আরও নির্দিষ্ট এবং বহুমুখী সময় পরিচালনার ক্ষমতা প্রদান করে।

এর সীমাবদ্ধতাগুলি সত্ত্বেও, C এর সময় ফাংশনগুলির সাদাসিদে এবং সার্বজনীনতা অনেক অ্যাপ্লিকেশনের জন্য এদের একদম উপযুক্ত করে তোলে। এই টুলগুলি বুঝতে পারা C প্রোগ্রামারদের জন্য মৌলিক, যা প্রোগ্রামিং এর ঐতিহাসিক পরিপ্রেক্ষিত এবং প্রায়শই ব্যবহৃত কার্যকারিতা একটি মিশ্রণ উপস্থাপন করে।
