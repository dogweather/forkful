---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:54:27.792498-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C-\u09A4\u09C7, \u09B2\u0997\u09BF\
  \u0982 \u09AE\u09CC\u09B2\u09BF\u0995 \u09AB\u09BE\u0987\u09B2 \u0985\u09AA\u09BE\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09BE \u0986\u09B0\u0993 \u0989\u09A8\u09CD\u09A8\
  \u09A4 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0985\u09B0\u09CD\
  \u099C\u09A8 \u0995\u09B0\u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\
  \u0964 \u09B8\u09BE\u09A6\u09BE\u09B8\u09BF\u09A7\u09C7 \u09AC\u09B0\u09CD\u09A3\
  \u09A8\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u0986\u09AE\u09B0\u09BE \u09AE\u09BE\
  \u09A8\u0995 I/O \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09A6\u09BF\
  \u09DF\u09C7 \u09B6\u09C1\u09B0\u09C1\u2026"
lastmod: '2024-04-05T21:53:53.276925-06:00'
model: gpt-4-0125-preview
summary: "C-\u09A4\u09C7, \u09B2\u0997\u09BF\u0982 \u09AE\u09CC\u09B2\u09BF\u0995\
  \ \u09AB\u09BE\u0987\u09B2 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AC\u09BE\
  \ \u0986\u09B0\u0993 \u0989\u09A8\u09CD\u09A8\u09A4 \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u0985\u09B0\u09CD\u099C\u09A8 \u0995\u09B0\u09BE \u09AF\
  \u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u09B8\u09BE\u09A6\u09BE\u09B8\
  \u09BF\u09A7\u09C7 \u09AC\u09B0\u09CD\u09A3\u09A8\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF, \u0986\u09AE\u09B0\u09BE \u09AE\u09BE\u09A8\u0995 I/O \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09A6\u09BF\u09DF\u09C7 \u09B6\u09C1\u09B0\
  \u09C1 \u0995\u09B0\u09AC\u0964 \u09A8\u09BF\u09AE\u09CD\u09A8\u09CB\u0995\u09CD\
  \u09A4 \u09B8\u09CD\u09A8\u09BF\u09AA\u09C7\u099F\u0997\u09C1\u09B2\u09BF \u09AE\
  \u09CC\u09B2\u09BF\u0995 \u09B2\u0997\u09BF\u0982 \u09AC\u09BE\u09B8\u09CD\u09A4\
  \u09AC\u09BE\u09DF\u09A8 \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8 \u0995\
  \u09B0\u09C7\u0964 \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u09AC\u09BE\u09B0\u09CD\
  \u09A4\u09BE \u09B2\u0997 \u0995\u09B0\u09A4\u09C7."
title: "\u09B2\u0997\u09BF\u0982"
weight: 17
---

## কিভাবে:
C-তে, লগিং মৌলিক ফাইল অপারেশন বা আরও উন্নত লাইব্রেরিগুলি ব্যবহার করে অর্জন করা যেতে পারে। সাদাসিধে বর্ণনার জন্য, আমরা মানক I/O লাইব্রেরি দিয়ে শুরু করব। নিম্নোক্ত স্নিপেটগুলি মৌলিক লগিং বাস্তবায়ন প্রদর্শন করে।

সাধারণ বার্তা লগ করতে:

```c
#include <stdio.h>

int main() {
    FILE *logFile;
    logFile = fopen("application.log", "a"); // লগ ফাইলটি অ্যাপেন্ড মোডে খোলা
    
    if (logFile == NULL) {
        perror("লগ ফাইল খোলার সমস্যা।");
        return -1;
    }
    
    fprintf(logFile, "এপ্লিকেশন শুরু।\n");
    
    // আপনার এপ্লিকেশনের লজিক এখানে
    
    fprintf(logFile, "এপ্লিকেশন সফলভাবে শেষ।\n");
    fclose(logFile);
    
    return 0;
}
```

`application.log`-এ আউটপুট:

```
এপ্লিকেশন শুরু।
এপ্লিকেশন সফলভাবে শেষ।
```

সময়সূচী এবং লগ লেভেলের সাথে আরও বিস্তারিত লগ অন্তর্ভুক্ত করতে:

```c
#include <stdio.h>
#include <time.h>

void logMessage(FILE *logFile, const char* level, const char* message) {
    time_t now;
    time(&now);
    char* datetime = ctime(&now);
    datetime[strlen(datetime)-1] = '\0'; // নিউলাইন ক্যারেক্টার সরান
    fprintf(logFile, "[%s] %s - %s\n", datetime, level, message);
}

int main() {
    FILE *logFile;
    logFile = fopen("detailed.log", "a");
    
    if (logFile == NULL) {
        perror("লগ ফাইল খোলার সমস্যা।");
        return -1;
    }
    
    logMessage(logFile, "INFO", "এপ্লিকেশন শুরু");
    // আপনার এপ্লিকেশনের লজিক এখানে
    logMessage(logFile, "ERROR", "একটি উদাহরণ ত্রুটি");
    
    fclose(logFile);
    
    return 0;
}
```

`detailed.log`-এ আউটপুট:

```
[Thu Mar 10 14:32:01 2023] INFO - এপ্লিকেশন শুরু
[Thu Mar 10 14:32:02 2023] ERROR - একটি উদাহরণ ত্রুটি
```

## গভীর ডুব
দেখানো হয়েছে, C তে লগিং মৌলিক ফাইল অপারেশনের উপর নির্ভর করে, যা কার্যকর কিন্তু অন্যান্য ভাষার লগিং সুবিধাদির মতো শক্তিশালী বা নমনীয় নয়, যেমন Python-এর `logging` মডিউল বা Java-র `Log4j`. সি-তে আরও উন্নত লগিং ক্ষমতার জন্য, ডেভেলপাররা প্রায়শই Unix-জাতীয় সিস্টেমে `syslog`-এর মতো লাইব্রেরিতে মুখ ফেরান, যা সিস্টেম-ব্যাপী লগ ম্যানেজমেন্ট প্রদান করে, অথবা `log4c` এর মতো থার্ড-পার্টি লাইব্রেরিগুলি ব্যবহার করে থাকে।

ঐতিহাসিকভাবে, লগিং প্রোগ্রামিংয়ের একটি অভিন্ন অংশ হিসেবে গণ্য হয়েছে, যেখানে প্রোগ্রামের প্রবাহ এবং ত্রুটি মূলত ফিজিক্যাল প্রিন্টআউটের মাধ্যমে ট্র্যাক এবং বোঝা হতো। সিস্টেমের বিকাশের সাথে সাথে লগিং আরও উন্নত হয়েছে, এখন বিভিন্ন গুরুত্বের লেভেল, লগ রোটেশন, এবং অ্যাসিঙ্ক্রোনাস লগিং সমর্থন করে।

যদিও C-এর মানক লাইব্রেরি লগিং বাস্তবায়নের জন্য মৌলিক সরঞ্জাম প্রদান করে, এর সীমাবদ্ধতা প্রায়শই কাস্টম লগিং ফ্রেমওয়ার্ক তৈরি করতে বা আরও বৈশিষ্ট্য-সমৃদ্ধ এবং নমনীয় লগিং সমাধানের জন্য বাহ্যিক লাইব্রেরি গ্রহণ করতে বাধ্য করে। এই সীমাবদ্ধতা সত্ত্বেও, ডিবাগিং এবং সফটওয়্যার রক্ষণাবেক্ষণে মৌলিক লগিং বাস্তবায়ন এবং বোঝা সিতে অপরিহার্য, বিশেষ করে যেখানে বাহ্যিক নির্ভরতা কমানো প্রয়োজন।
