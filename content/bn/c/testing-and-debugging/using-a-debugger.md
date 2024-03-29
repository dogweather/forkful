---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:22:57.469118-06:00
description: "C \u09A4\u09C7 \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0\u0997\u09C1\
  \u09B2\u09BF \u09B9\u09B2\u09CB \u09AC\u09BF\u09B6\u09C7\u09B7\u09BE\u09AF\u09BC\
  \u09BF\u09A4 \u09AF\u09A8\u09CD\u09A4\u09CD\u09B0, \u09AF\u09BE \u09A1\u09C7\u09AD\
  \u09C7\u09B2\u09AA\u09BE\u09B0\u09A6\u09C7\u09B0 \u09A4\u09BE\u09A6\u09C7\u09B0\
  \ \u0995\u09CB\u09A1 \u09A6\u09BF\u09AF\u09BC\u09C7 \u09A7\u09BE\u09AA\u09C7 \u09A7\
  \u09BE\u09AA\u09C7 \u098F\u0997\u09CB\u09A4\u09C7, \u09AD\u09C7\u09B0\u09BF\u09AF\
  \u09BC\u09C7\u09AC\u09B2\u0997\u09C1\u09B2\u09BF \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\
  \u09BE \u0995\u09B0\u09A4\u09C7, \u098F\u09AC\u0982 \u09A8\u09BF\u09B0\u09CD\u09AC\
  \u09BE\u09B9\u09C7\u09B0 \u09AA\u09CD\u09B0\u09AC\u09BE\u09B9 \u09A8\u09BF\u09B0\
  \u09C0\u0995\u09CD\u09B7\u09A3 \u0995\u09B0\u09A4\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.547239-06:00'
model: gpt-4-0125-preview
summary: "C \u09A4\u09C7 \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0\u0997\u09C1\u09B2\
  \u09BF \u09B9\u09B2\u09CB \u09AC\u09BF\u09B6\u09C7\u09B7\u09BE\u09AF\u09BC\u09BF\
  \u09A4 \u09AF\u09A8\u09CD\u09A4\u09CD\u09B0, \u09AF\u09BE \u09A1\u09C7\u09AD\u09C7\
  \u09B2\u09AA\u09BE\u09B0\u09A6\u09C7\u09B0 \u09A4\u09BE\u09A6\u09C7\u09B0 \u0995\
  \u09CB\u09A1 \u09A6\u09BF\u09AF\u09BC\u09C7 \u09A7\u09BE\u09AA\u09C7 \u09A7\u09BE\
  \u09AA\u09C7 \u098F\u0997\u09CB\u09A4\u09C7, \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\
  \u09C7\u09AC\u09B2\u0997\u09C1\u09B2\u09BF \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE\
  \ \u0995\u09B0\u09A4\u09C7, \u098F\u09AC\u0982 \u09A8\u09BF\u09B0\u09CD\u09AC\u09BE\
  \u09B9\u09C7\u09B0 \u09AA\u09CD\u09B0\u09AC\u09BE\u09B9 \u09A8\u09BF\u09B0\u09C0\
  \u0995\u09CD\u09B7\u09A3 \u0995\u09B0\u09A4\u09C7\u2026"
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

C তে ডিবাগারগুলি হলো বিশেষায়িত যন্ত্র, যা ডেভেলপারদের তাদের কোড দিয়ে ধাপে ধাপে এগোতে, ভেরিয়েবলগুলি পরীক্ষা করতে, এবং নির্বাহের প্রবাহ নিরীক্ষণ করতে সমর্থন করে। বাগ চিহ্নিত করে এবং ঠিক করে, নিশ্চিত করে যে কোডটি প্রত্যাশিতভাবে আচরণ করে, এই প্রক্রিয়া অন্তর্ভুক্ত।

## কিভাবে:

GDB (GNU Debugger) হল C প্রোগ্রামিং-এর জন্য সবচেয়ে প্রচলিত ব্যবহৃত ডিবাগার। একটি সাধারণ C প্রোগ্রাম ডিবাগ করার জন্য GDB ব্যবহার করে একটি সংক্ষিপ্ত গাইড এখানে দেওয়া হলো।

প্রথমে, আপনার C প্রোগ্রামটি `-g` ফ্ল্যাগের সাথে কম্পাইল করুন যাতে ডিবাগিং তথ্য সংযুক্ত হয়:

```c
gcc -g program.c -o program
```

পরবর্তীতে, আপনার কম্পাইলড প্রোগ্রামের সাথে GDB চালু করুন:

```bash
gdb ./program
```

আপনি এখন GDB এর অভ্যন্তরে বিভিন্ন কমান্ড ব্যবহার করে এর অপারেশন নিয়ন্ত্রণ করতে পারেন। এখানে কিছু মৌলিক কমান্ড দেওয়া হলো:

- `break`: নির্দিষ্ট একটি লাইন অথবা ফাংশনে একটি ব্রেকপয়েন্ট সেট করে নির্বাহ থামান।
  - উদাহরণ: `break 10` অথবা `break main`
- `run`: GDB এর ভেতরে আপনার প্রোগ্রামের নির্বাহ শুরু করুন।
- `next`: ফাংশনের ভেতরে প্রবেশ না করে পরবর্তী লাইনের কোড নির্বাহ করুন।
- `step`: ফাংশনের মধ্যে প্রবেশ করে পরবর্তী লাইনের কোড নির্বাহ করুন।
- `print`: একটি ভেরিয়েবলের মান প্রদর্শন করুন।
- `continue`: পরবর্তী ব্রেকপয়েন্ট পর্যন্ত নির্বাহ চালিয়ে যান।
- `quit`: GDB থেকে বের হয়ে আসুন।

এখানে একটি সাধারণ প্রোগ্রাম ডিবাগ করার একটি নমুনা অধিবেশনের উদাহরণ দেওয়া হলো:

```c
#include <stdio.h>

int main() {
    int i;
    for (i = 0; i < 5; i++) {
        printf("%d\n", i);
    }
    return 0;
}
```

বর্ণনা অনুযায়ী কম্পাইল করুন এবং GDB চালু করুন। `printf` লাইনে `break 5` দিয়ে একটি ব্রেকপয়েন্ট সেট করুন এবং তারপর `run` করুন। লুপ ধাপে ধাপে এগিয়ে নিতে `next` ব্যবহার করুন এবং লুপ ভেরিয়েবল পরীক্ষা করতে `print i` ব্যবহার করুন।

প্রথম ইটারেশনের আগে একটি ব্রেকপয়েন্ট সেট করার পরে নমুনা আউটপুট:

```
Breakpoint 1, main () at program.c:5
5         printf("%d\n", i);
```

কয়েকটি ইটারেশনের পরে `print i` ব্যবহার করে:

```
$3 = 2
```

এটি একটি সাধারণ প্রোগ্রামের অবস্থা এবং প্রবাহ পরীক্ষা করার প্রক্রিয়া দেখায়।

## গভীরে:

ডিবাগিং এর ধারণা প্রোগ্রামিং এর প্রাথমিক দিনগুলিতে অনেক উন্নত হয়েছে, যেখানে আক্ষরিক বাগ (আসলে পোকামাকড়) মেকানিকাল কম্পিউটারগুলিতে সমস্যা সৃষ্টি করতে পারে। আজ, GDB এর মতো ডিবাগারগুলি মৌলিক ধাপে ধাপে নির্বাহ এবং ভেরিয়েবল পরীক্ষার বেশি, যেমন বিপরীত ডিবাগিং (প্রোগ্রাম পিছনের দিকে নির্বাহ করা), শর্তসাপেক্ষ ব্রেকপয়েন্ট, এবং স্বয়ংক্রিয় ডিবাগিং কাজের জন্য স্ক্রিপ্টিং-এর মতো উন্নত বৈশিষ্ট্য সরবরাহ করে।

যদিও GDB শক্তিশালী এবং ব্যাপকভাবে ব্যবহৃত হয়, এটি শুরুকারীদের জন্য ঘন এবং চ্যালেঞ্জিং হতে পারে। Visual Studio Code, CLion, অথবা Eclipse এর মতো বিকল্প ডিবাগিং টুল এবং IDEs (ইন্টিগ্রেটেড ডেভেলপমেন্ট এনভায়রনমেন্টস) ডিবাগিং C কোডের জন্য আরও ব্যবহারকারী-বান্ধব ইন্টারফেস সরবরাহ করে, প্রায়শই দৃশ্যমান সাহায্য এবং আরও সহজাত নিয়ন্ত্রণ একীকরণ করে। এই বিকল্পগুলি GDB এর পুরো গভীরতার কার্যকারিতা সরবরাহ নাও করতে পারে, তবে এগুলি C প্রোগ্রামিং-এ নতুনদের জন্য আরও অ্যা�
