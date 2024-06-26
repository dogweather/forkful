---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:01.944287-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B0\
  \ `String` \u0995\u09CD\u09B2\u09BE\u09B8\u09C7 \u098F\u0995\u099F\u09BF \u09A6\u09BE\
  \u09B0\u09C1\u09A3 `toLowerCase()` \u09AE\u09C7\u09A5\u09A1 \u09B0\u09AF\u09BC\u09C7\
  \u099B\u09C7 \u09AF\u09C7\u099F\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u0995\u09A0\u09BF\u09A8 \u0995\u09BE\u099C\u099F\u09BF \u09B8\u09B9\
  \u099C \u0995\u09B0\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u098F\u0987 \u09B8\u09B9\
  \u099C \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u098F\u0995\u099F\
  \u09BF \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0996\u09C1\u09A8."
lastmod: '2024-03-17T18:47:43.890258-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B0 `String` \u0995\u09CD\u09B2\u09BE\u09B8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09A6\u09BE\u09B0\u09C1\u09A3 `toLowerCase()` \u09AE\
  \u09C7\u09A5\u09A1 \u09B0\u09AF\u09BC\u09C7\u099B\u09C7 \u09AF\u09C7\u099F\u09BF\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09A0\u09BF\u09A8\
  \ \u0995\u09BE\u099C\u099F\u09BF \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A6\u09C7\
  \u09AF\u09BC\u0964 \u098F\u0987 \u09B8\u09B9\u099C \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u0989\u09A6\u09BE\u09B9\u09B0\
  \u09A3 \u09A6\u09C7\u0996\u09C1\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কিভাবে:
জাভার `String` ক্লাসে একটি দারুণ `toLowerCase()` মেথড রয়েছে যেটি আপনার জন্য কঠিন কাজটি সহজ করে দেয়। এই সহজ ব্যবহারের একটি উদাহরণ দেখুন:

```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String original = "Java ROCKS!";
        String lowerCased = original.toLowerCase();
        System.out.println(lowerCased);
    }
}
```

আউটপুট:

```
java rocks!
```

ব্যস, স্ট্রিংটি তার আওয়াজকে একটি শান্ত ছোট অক্ষরে নিচে নামিয়ে আনে।

## গভীরে ভ্রমণ
এক সময়, টেক্সট সামলানো একটি কঠিন ব্যাপার ছিল। ভিন্ন ভাষা, ভিন্ন কেস, কম্পিউটার সিস্টেমগুলি বিভ্রান্তিতে চিত্কার করত। '90-এর দশকে জাভা দৃশ্যে আসে এবং জিনিসগুলিকে সহজ করে তোলার চেষ্টা করে। `toLowerCase()` মেথডটি জাভার `String` ক্লাসের প্রারম্ভিক দিনগুলিতে থেকে এসেছে।

তবে অভ্যন্তরে কিছু বিস্ময়কর ব্যাপার আছে। আপনি ভাবতে পারেন, `toLowerCase()` কেন প্রয়োজন। ব্যাপারটি হল, সকল সংস্কৃতি "ছোট অক্ষর" একইভাবে সংজ্ঞায়িত করে না। এই মেথডটি লোকেল-সেনসিটিভ, আপনার সিস্টেমের ডিফল্ট লোকেল ব্যবহার করে, অথবা আপনি `toLowerCase(Locale locale)` ব্যবহার করে একটি নির্দিষ্ট করতে পারেন।

আরেকটি মোড়: তুর্কি মতো আরও বিস্তৃত লিপির ভাষাগুলিতে, বিশেষ "ডটবিহীন" i অক্ষর রয়েছে যেগুলি সাধারণ লোয়ার-কেসিংকে ব্যাহত করতে পারে। অতএব, জাভা অক্ষর রূপান্তরের বিষয়ে যত্নশীল থাকার বিকল্প সরবরাহ করে।

বিকল্প? নিশ্চয়ই, আপনি একটি `for` লুপের সাহায্যে স্ট্রিংটি দিয়ে চারপাশ দিয়ে অক্ষরগুলি ম্যানুয়ালি প্রতিস্থাপন করতে পারেন। কিন্তু যখন জাভার প্রস্তাব অনুসরণ করা সহজ, তখন কেন নতুন কিছু আবিষ্কার করতে যাবেন?

আরও, এটি কিছুকে অবাক করতে পারে: জাভায় স্ট্রিংগুলি অপরিবর্তনীয়। যখন আপনি `toLowerCase()` ব্যবহার করেন, আপনি মূল স্ট্রিংটি পরিবর্তন করছেন না, আপনি একটি নতুন স্ট্রিং তৈরি করছেন, ভেস্ট সহ।

## দেখুন আরও
আপনার স্ট্রিং গেম উন্নত করার জন্য এই সম্পদগুলি দেখুন:

- জাভা স্ট্রিং API: [](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- জাভা লোকেল ক্লাস: [](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Locale.html)
- ইউনিকোড কেস ম্যাপিংস: [](https://unicode.org/reports/tr21/)

এবং ইউনিকোড স্ট্যান্ডার্ডের বিস্তারিত বিবরণের জন্য:

- দ্য ইউনিকোড কনসোর্টিয়াম: [](https://unicode.org/)
