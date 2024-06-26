---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:31.791977-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B0\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u09A4\u09C7\
  \ \u098F\u0995\u099F\u09BF `length()` \u09AE\u09C7\u09A5\u09A1 \u0986\u099B\u09C7\
  \u0964 \u098F\u099F\u09BF \u09A1\u09BE\u0995\u09B2\u09C7, \u0986\u09AA\u09A8\u09BF\
  \ \u0985\u0995\u09CD\u09B7\u09B0\u09C7\u09B0 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \u099F\u09BF \u09AA\u09C7\u09DF\u09C7 \u09AF\u09BE\u09AC\u09C7\u09A8\u0964 \u09B8\
  \u09B9\u099C\u0964."
lastmod: '2024-03-17T18:47:43.894672-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u098F\u0995\u099F\u09BF `length()` \u09AE\u09C7\
  \u09A5\u09A1 \u0986\u099B\u09C7\u0964 \u098F\u099F\u09BF \u09A1\u09BE\u0995\u09B2\
  \u09C7, \u0986\u09AA\u09A8\u09BF \u0985\u0995\u09CD\u09B7\u09B0\u09C7\u09B0 \u09B8\
  \u0982\u0996\u09CD\u09AF\u09BE\u099F\u09BF \u09AA\u09C7\u09DF\u09C7 \u09AF\u09BE\
  \u09AC\u09C7\u09A8\u0964 \u09B8\u09B9\u099C\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
weight: 7
---

## কিভাবে:
জাভার স্ট্রিংগুলিতে একটি `length()` মেথড আছে। এটি ডাকলে, আপনি অক্ষরের সংখ্যাটি পেয়ে যাবেন। সহজ।

```java
public class StringLengthExample {
    public static void main(String[] args) {
        String greeting = "Hello, World!";
        int length = greeting.length();

        System.out.println("স্ট্রিংয়ের দৈর্ঘ্য: " + length);
        // আউটপুট: স্ট্রিংয়ের দৈর্ঘ্য: 13
    }
}
```

## গভীরে যাও
`length()` মেথডটি জাভার প্রাথমিক ভার্সনগুলিতে প্রবর্তিত ছিল, যা এটিকে `String` ক্লাসের একটি দীর্ঘস্থায়ী অংশ করে তোলে। এটি সহজ তবে অপরিহার্য। অন্তর্নিহিতভাবে, জাভায় একটি `String` একটি অক্ষরের অ্যারে দ্বারা সমর্থিত হয়, এবং `length()` মেথডটি এই অ্যারের আকার ফেরত দেয়। সংক্ষিপ্তভাবে, জাভা স্ট্রিংগুলি অপরিবর্তনশীল, তাই একবার তৈরি হওয়ার পর, দৈর্ঘ্য পরিবর্তন হয় না, যা এই মেথডটিকে দ্রুত এবং নির্ভরযোগ্য করে তোলে।

বিকল্প? না, অক্ষরগণনা করার জন্য নিজের একটি ফাংশন তৈরি করা ছাড়া (যা অনর্থক এবং অকার্যকর), তেমন কোনো কিছু নেই। মাথায় রাখবেন `length()` সাধারণ 16-বিট `char` আকারের মানে না থাকা ইউনিকোড অক্ষরের জন্য নম্বর `char` ইউনিট ফেরত দেয়, অতিরিক্ত অক্ষরগুলির জন্য `codePointCount()` ব্যবহার করতে বিবেচনা করুন।

## আরও দেখুন
গভীরে ডুব দিন অথবা সম্পর্কিত বিষয়াবলী অন্বেষণ করুন:
- [জাভা স্ট্রিং ডকুমেন্টেশন](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [জাভা ক্যারেক্টার ক্লাস ডক্স](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Character.html) ইউনিকোড, অক্ষর, এবং কোড পয়েন্টের উপর আরও বুঝতে।
- [ওরাকল'স জাভা টিউটোরিয়ালস](https://docs.oracle.com/javase/tutorial/java/data/strings.html) জাভাতে স্ট্রিং সম্পর্কে আরও বিস্তারিত জানার জন্য।
