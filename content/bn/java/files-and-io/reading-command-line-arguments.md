---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:58.409959-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u0995\u09AE\u09BE\
  \u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\
  \u09C7\u09A8\u09CD\u099F\u09B8\u0997\u09C1\u09B2\u09BF `main` \u09AE\u09C7\u09A5\
  \u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0997\u09CD\u09B0\u09B9\u09A3 \u0995\
  \u09B0\u09C7\u0964 \u098F\u0987 \u0996\u09C1\u0981\u099F\u09BF\u09A8\u09BE\u099F\
  \u09BF \u09A8\u09AE\u09C1\u09A8\u09BE\u099F\u09BF \u09A6\u09C7\u0996\u09C1\u09A8\
  ."
lastmod: '2024-03-17T18:47:43.920292-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE \u0986\u09AA\u09A8\u09BE\u09B0 \u09A6\u09C7\u0993\
  \u09AF\u09BC\u09BE \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8\
  \ \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u09B8\u0997\u09C1\
  \u09B2\u09BF `main` \u09AE\u09C7\u09A5\u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0997\u09CD\u09B0\u09B9\u09A3 \u0995\u09B0\u09C7\u0964 \u098F\u0987 \u0996\u09C1\
  \u0981\u099F\u09BF\u09A8\u09BE\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE\u099F\u09BF\
  \ \u09A6\u09C7\u0996\u09C1\u09A8."
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

## কিভাবে:
জাভা আপনার দেওয়া কমান্ড লাইন আর্গুমেন্টসগুলি `main` মেথডের সাথে গ্রহণ করে। এই খুঁটিনাটি নমুনাটি দেখুন:

```java
public class CommandLineExample {
    public static void main(String[] args) {
        // চলুন কমান্ড লাইন আর্গুমেন্টসগুলি প্রিন্ট আউট করি
        for(String arg : args) {
            System.out.println(arg);
        }
    }
}
```

আপনার টার্মিনাল চালু করুন, `javac CommandLineExample.java` দিয়ে কম্পাইল করুন, এবং `java CommandLineExample These Are Command Line Arguments` দিয়ে রান করুন। এখানে আপনার আউটপুট:

```
These
Are
Command
Line
Arguments
```

## গভীরে ডুব:
C থেকে উৎসারিত, কমান্ড লাইন আর্গুমেন্টস প্রোগ্রামিং-এর অন্ধকার যুগ থেকে—পাঞ্চ কার্ড এবং টাইমশেয়ারিং চিন্তা করুন—একটি মূল উপাদান হয়ে রেখেছে। জাভা এই ব্যবহারিকতা ভাল কারণে উত্তরাধিকার হিসেবে পেয়েছে। এটি প্রাথমিক, বহুমুখী এবং বিভিন্ন পরিস্থিতিতে ফিট বসে।

উচ্চতর বিকল্প? অবশ্যই, অনেক কিছু আছে। জেকম্যান্ডার বা অ্যাপাচি কমন্স CLI এর মতো লাইব্রেরিগুলি আপনার পার্সিং ক্ষমতাকে বৃদ্ধি করে। তারা আরও জটিল পরিস্থিতিগুলি সুকৌশলে সামলায়।

অন্তরালে, জাভার `main` মেথড একটি `String` অ্যারে—`args` গ্রহণ করে। ভার্চুয়াল মেশিন চালু হলে, যখন আপনি `java ClassName` মারেন, তারপরে আপনার ইনপুটগুলি, যা `args` এ নিপুণভাবে সংরক্ষিত হয়।

## দেখুন এছাড়াও:
- বেসিকস নিয়ে একটি রিফ্রেশারের জন্য: [অরাকলের অফিশিয়াল জাভা টিউটোরিয়ালস](https://docs.oracle.com/javase/tutorial/)
- জটিল পার্সিংয়ের জন্য জেকম্যান্ডকে ডুব দিন: [জেকম্যান্ডার গিটহাব](https://github.com/cbeust/jcommander)
- অ্যাপাচি কমন্স CLI অন্বেষণ করুন: [অ্যাপাচি কমন্স CLI](https://commons.apache.org/proper/commons-cli/)
