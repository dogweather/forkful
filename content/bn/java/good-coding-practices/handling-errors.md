---
title:                "ত্রুটিগুলি পরিচালনা করা"
date:                  2024-03-17T17:50:45.916356-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
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
