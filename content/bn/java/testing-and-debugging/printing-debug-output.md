---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:07:11.289950-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: IDE-\u0997\u09C1\u09B2\u09BF \u099A\
  \u09BE\u09B2\u09BE\u0995 \u09B9\u0993\u09AF\u09BC\u09BE\u09B0 \u0986\u0997\u09C7\
  , printf-\u09B8\u09CD\u099F\u09BE\u0987\u09B2 \u09A1\u09BF\u09AC\u09BE\u0997\u09BF\
  \u0982 \u099B\u09BF\u09B2 \u09AF\u09BE\u0993\u09AF\u09BC\u09BE\u09B0 \u09AA\u09A5\
  \u0964 \u098F\u0996\u09A8\u0993, \u09AB\u09CD\u09AF\u09BE\u09A8\u09CD\u09B8\u09BF\
  \ \u09AC\u09CD\u09B0\u09C7\u0995\u09AA\u09AF\u09BC\u09C7\u09A8\u09CD\u099F\u09C7\
  \u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7, \u0995\u0996\u09A8\u0993 \u0995\u0996\u09A8\
  \u0993 \u098F\u0995\u099F\u09BF \u09AD\u09BE\u09B2\u09AD\u09BE\u09AC\u09C7 \u09B8\
  \u09CD\u09A5\u09BE\u09AA\u09BF\u09A4\u2026"
lastmod: '2024-04-05T22:51:04.887956-06:00'
model: gpt-4-0125-preview
summary: "IDE-\u0997\u09C1\u09B2\u09BF \u099A\u09BE\u09B2\u09BE\u0995 \u09B9\u0993\
  \u09AF\u09BC\u09BE\u09B0 \u0986\u0997\u09C7, printf-\u09B8\u09CD\u099F\u09BE\u0987\
  \u09B2 \u09A1\u09BF\u09AC\u09BE\u0997\u09BF\u0982 \u099B\u09BF\u09B2 \u09AF\u09BE\
  \u0993\u09AF\u09BC\u09BE\u09B0 \u09AA\u09A5\u0964 \u098F\u0996\u09A8\u0993, \u09AB\
  \u09CD\u09AF\u09BE\u09A8\u09CD\u09B8\u09BF \u09AC\u09CD\u09B0\u09C7\u0995\u09AA\u09AF\
  \u09BC\u09C7\u09A8\u09CD\u099F\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7, \u0995\
  \u0996\u09A8\u0993 \u0995\u0996\u09A8\u0993 \u098F\u0995\u099F\u09BF \u09AD\u09BE\
  \u09B2\u09AD\u09BE\u09AC\u09C7 \u09B8\u09CD\u09A5\u09BE\u09AA\u09BF\u09A4 `System.out.println()`\
  \ \u09AA\u09B0\u09CD\u09AF\u09BE\u09AA\u09CD\u09A4 \u09B9\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7 \u0997\u09CD\u09B0\u09B9\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B8\
  \u09BE\u09B0\u09BF\u09AC\u09A6\u09CD\u09A7 \u0995\u09B0\u09A4\u09C7\u0964 \u09AC\
  \u09BF\u0995\u09B2\u09CD\u09AA?"
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কিভাবে:
চলুন কিছু কোড পর্দায় দেখাই:

```java
public class DebugExample {
    public static void main(String[] args) {
        int sum = 0;
        for (int i = 1; i <= 10; i++) {
            sum += i;
            System.out.println("যোগ করা হল " + i + ", এখন সমূহঃ " + sum);
        }
    }
}
```

এই স্নিপেটটি 1 থেকে 10 পর্যন্ত সংখ্যা যোগ করে এবং অগ্রগতি প্রিন্ট করে:

```
যোগ করা হল 1, এখন সমূহঃ 1
যোগ করা হল 2, এখন সমূহঃ 3
...
যোগ করা হল 10, এখন সমূহঃ 55
```

## গভীর ডুব
IDE-গুলি চালাক হওয়ার আগে, printf-স্টাইল ডিবাগিং ছিল যাওয়ার পথ। এখনও, ফ্যান্সি ব্রেকপয়েন্টের মধ্যে, কখনও কখনও একটি ভালভাবে স্থাপিত `System.out.println()` পর্যাপ্ত হতে পারে গ্রহগুলিকে সারিবদ্ধ করতে।

বিকল্প? লগিং ফ্রেমওয়ার্ক যেমন Log4J বা SLF4J আপনাকে ডিবাগ তথ্য নিয়ন্ত্রণ করতে, এটি সিস্টেম আউটপুট থেকে আলাদা করতে এবং আপনাকে বাক্যালাপতা টগল করতে দেয়।

বাস্তবায়নের দিক থেকে, মনে রাখবেন যে `System.out` একটি `PrintStream` অবজেক্ট, যা ডিফল্টরূপে stdout এ যায়। এটি আউটপুট পুনঃনির্দেশ করতে প্রতিস্থাপিত হতে পারে, পরীক্ষা বা লগিংকে কম হস্তক্ষেপমূলক করে তোলে।

## দেখুন এছাড়াও
- [অরাকলের I/O স্ট্রিমস সম্পর্কে টিউটোরিয়াল](https://docs.oracle.com/javase/tutorial/essential/io/streams.html)
- [জাভায় লগিং এর সেরা অনুশীলন](https://www.baeldung.com/java-logging-intro)
- [SLF4J ডকুমেন্টেশন](http://www.slf4j.org/docs.html)
