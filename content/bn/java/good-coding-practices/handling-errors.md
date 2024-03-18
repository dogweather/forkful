---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:45.916356-06:00
description: "\u09AD\u09C1\u09B2\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AE\u09CB\
  \u0995\u09BE\u09AC\u09C7\u09B2\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u098F\u09AE\u09A8 \u0995\u09CB\u09A1 \u09B2\u09C7\u0996\u09BE \u09AF\u09BE \u09AD\
  \u09C1\u09B2 \u09B9\u09AF\u09BC\u09C7 \u0997\u09C7\u09B2\u09C7 \u09A4\u09BE\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u09AE\u09CB\u0995\u09BE\u09AC\u09C7\u09B2\u09BE \u0995\
  \u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\
  \u09C7 \u09B8\u09AB\u09CD\u099F\u0993\u09AF\u09BC\u09CD\u09AF\u09BE\u09B0\u0995\u09C7\
  \ \u09A6\u09C3\u09A2\u09BC \u0995\u09B0\u09C7 \u09A4\u09C1\u09B2\u09A4\u09C7, \u0995\
  \u09CD\u09B0\u09CD\u09AF\u09BE\u09B6 \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:43.912223-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09C1\u09B2\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AE\u09CB\u0995\
  \u09BE\u09AC\u09C7\u09B2\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\
  \u09AE\u09A8 \u0995\u09CB\u09A1 \u09B2\u09C7\u0996\u09BE \u09AF\u09BE \u09AD\u09C1\
  \u09B2 \u09B9\u09AF\u09BC\u09C7 \u0997\u09C7\u09B2\u09C7 \u09A4\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AE\u09CB\u0995\u09BE\u09AC\u09C7\u09B2\u09BE \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\
  \ \u09B8\u09AB\u09CD\u099F\u0993\u09AF\u09BC\u09CD\u09AF\u09BE\u09B0\u0995\u09C7\
  \ \u09A6\u09C3\u09A2\u09BC \u0995\u09B0\u09C7 \u09A4\u09C1\u09B2\u09A4\u09C7, \u0995\
  \u09CD\u09B0\u09CD\u09AF\u09BE\u09B6 \u098F\u09AC\u0982\u2026"
title: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u0997\u09C1\u09B2\u09BF \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?

ভুলের সাথে মোকাবেলা করা মানে এমন কোড লেখা যা ভুল হয়ে গেলে তার সাথে মোকাবেলা করতে পারে। প্রোগ্রামাররা এটি করে সফ্টওয়্যারকে দৃঢ় করে তুলতে, ক্র্যাশ এবং অদ্ভুত আচরণ থেকে রক্ষা করতে।

## কিভাবে:

জাভা ভুলের সাথে মোকাবেলা করতে ব্যতিক্রম (exceptions) ব্যবহার করে। আপনি ঝুঁকিপূর্ণ কোডকে `try` ব্লক দিয়ে বেষ্টন করুন এবং ব্যতিক্রমের সাথে `catch` দ্বারা ধরুন। এখানে একটি সাধারণ উদাহরণ দেওয়া হলঃ

```java
public class ErrorHandlingExample {
    public static void main(String[] args) {
        try {
            int result = divide(10, 0);
            System.out.println("ফলাফল হলো: " + result);
        } catch (ArithmeticException e) {
            System.out.println("ওহ্‌, শূন্য দিয়ে ভাগ করা যায় না!");
        }
    }

    private static int divide(int অংশীদার, int ভাজক) {
        return অংশীদার / ভাজক;
    }
}
```

আউটপুট:
```
ওহ্‌, শূন্য দিয়ে ভাগ করা যায় না!
```

## গভীরে ডুব:

জাভায় ত্রুটি সামলানোর প্রক্রিয়া বিকশিত হয়েছে। প্রাথমিক সময়ে ব্যতিক্রম ছিল না; প্রোগ্রামাররা ত্রুটি কোড পরীক্ষা করত। তারপর জাভা ট্রাই-ক্যাচ ব্লক প্রবর্তন করে, যা ত্রুটি সামলানোকে আরও মার্জিত করে তোলে।

ঐতিহ্যগত `try-catch` এর বিকল্প হিসেবে জাভা 7 এতে প্রবর্তিত `try-with-resources` রয়েছে যা সম্পদ স্বয়ংক্রিয়ভাবে বন্ধ করতে এবং কোডকে পরিষ্কার রাখতে সাহায্য করে।

বাস্তবায়নের বিস্তারিত গুরুত্বপূর্ণ। উদাহরনস্বরূপ, `Exception` বা `Throwable` ধরাটা সাধারণত খারাপ অনুশীলন। এটি অতিরিক্ত ব্যাপক, আপনি সম্ভবত অবগত না থাকা বাগগুলি ঢেকে রাখে। নির্দিষ্ট ব্যতিক্রমে লেগে থাকুন।

## আরও দেখুন

- ব্যতিক্রম সম্পর্কে অফিসিয়াল ওরাকল জাভা টিউটোরিয়াল: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- জাভার `try-with-resources` বিবৃতি ডকুমেন্টেশন: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- ব্যতিক্রমের উপর সেরা অনুশীলন জানতে জশুয়া ব্লোচের লেখা Effective Java.
