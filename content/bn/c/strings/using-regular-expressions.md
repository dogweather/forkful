---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:26:17.599670-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: C \u09AD\u09BE\u09B7\u09BE\u09AF\
  \u09BC \u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\
  \u09CD\u09B0\u09C7\u09B6\u09A8\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\u09BF \u09AA\u09CD\u09B0\u09BE\u09A5\
  \u09AE\u09BF\u0995\u09AD\u09BE\u09AC\u09C7 POSIX regex \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF (`<regex.h>`) \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09AC\u09C7\u09A8\u0964 \u098F\u0987 \u0989\u09A6\u09BE\
  \u09B9\u09B0\u09A3\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u09AA\u09CD\u09AF\
  \u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.530821-06:00'
model: gpt-4-0125-preview
summary: "C \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC \u09B0\u09C7\u0997\u09C1\u09B2\u09BE\
  \u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\u09B0\u09C7\u09B6\u09A8\u09B8 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\
  \u09BF \u09AA\u09CD\u09B0\u09BE\u09A5\u09AE\u09BF\u0995\u09AD\u09BE\u09AC\u09C7\
  \ POSIX regex \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF (`<regex.h>`)\
  \ \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09AC\u09C7\
  \u09A8\u0964 \u098F\u0987 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\u099F\u09BF \u09AE\
  \u09CC\u09B2\u09BF\u0995 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\
  \ \u09AE\u09BF\u09B2\u09BE\u09A8 \u09A6\u09C7\u0996\u09BE\u09AF\u09BC."
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

## কীভাবে:
C ভাষায় রেগুলার এক্সপ্রেশনস ব্যবহার করতে, আপনি প্রাথমিকভাবে POSIX regex লাইব্রেরি (`<regex.h>`) এর সাথে কাজ করবেন। এই উদাহরণটি মৌলিক প্যাটার্ন মিলান দেখায়:

```c
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main(){
    regex_t regex;
    int return_value;
    char *pattern = "^a[[:alnum:]]"; // 'a' দিয়ে শুরু হওয়া পরে আলফানিউমেরিক অক্ষর অনুসরণ করা স্ট্রিংগুলির জন্য প্যাটার্ন
    char *test_string = "apple123";

    // নিয়মিত এক্সপ্রেশন কম্পাইল করুন
    return_value = regcomp(&regex, pattern, REG_EXTENDED);
    if (return_value) {
        printf("Could not compile regex\n");
        exit(1);
    }

    // নিয়মিত এক্সপ্রেশন নির্বাহ করুন
    return_value = regexec(&regex, test_string, 0, NULL, 0);
    if (!return_value) {
        printf("Match found\n");
    } else if (return_value == REG_NOMATCH) {
        printf("No match found\n");
    } else {
        printf("Regex match failed\n");
        exit(1);
    }

    // regex ব্যবহৃত মেমোরি মুক্ত করুন
    regfree(&regex);

    return 0;
}
```

একটি মেলে যাওয়া স্ট্রিংয়ের জন্য নমুনা আউটপুট ("apple123"):
```
Match found
```
এবং একটি অমেলে যাওয়া স্ট্রিংয়ের জন্য ("banana"):
```
No match found
```

## গভীর ডাইভ:
C ভাষায় রেগুলার এক্সপ্রেশনস, POSIX মানদণ্ডের অংশ হিসাবে, স্ট্রিং মিলান এবং ম্যানিপুলেশন সম্পাদনের একটি মজবুত উপায় অফার করে। যাইহোক, C ভাষায় POSIX regex লাইব্রেরির API, Python বা Perl এর মতো প্রথম শ্রেণির স্ট্রিং ম্যানিপুলেশন বৈশিষ্ট্যের সাথে নকশা করা ভাষাগুলিতে পাওয়া API এর তুলনায় বেশি বিব্রতকর মনে করা হয়। প্যাটার্নের জন্য সিনট্যাক্স ভাষা জুড়ে অনুরূপ, কিন্তু C এ ম্যানুয়াল মেমোরি ম্যানেজমেন্ট এবং regex প্যাটার্নগুলি ব্যবহার করে প্রস্তুতি, নির্বাহ, এবং পরবর্তী পরিষ্কারের জন্য আরো বেশি বয়লারপ্লেট কোডের প্রয়োজন হয়।

এই চ্যালেঞ্জ সত্ত্বেও, C ভাষায় regex ব্যবহার করতে শিখা পুরস্কারস্বরূপ কারণ এটি নিম্ন-স্তরের প্রোগ্রামিং ধারণার উপর গভীর অন্তর্দৃষ্টি দান করে। এছাড়াও, এটি C প্রোগ্রামিংকে টেক্সট প্রসেসিং এবং ডেটা এক্সট্রাকশনের মতো অঞ্চলের জন্য সম্ভাবনা খুলে দেয় যেখানে regex অপরিহার্য। আরো জটিল প্যাটার্ন বা regex অপারেশনগুলির জন্য, PCRE (Perl Compatible Regular Expressions) লাইব্রেরি আরো বৈশিষ্ট্যযুক্ত এবং কঠোর সহজ ইন্টারফেস অফার করতে পারে, যদিও এটি আপনার C প্রোজেক্টে একটি বহিরাগত লাইব্রেরি একীভূত করে।
