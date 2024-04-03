---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:55.915441-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE `String.format()`\
  \ \u098F\u09A8\u09CD\u099F\u09BE\u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8\u09C7\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09AC\u09B0\u09CD\u09A4\u09A8\
  \ \u0995\u09B0\u09C7\u099B\u09C7."
lastmod: '2024-03-17T18:47:43.888987-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE `String.format()` \u098F\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u09AA\u09CD\u09B0\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09C7\u099B\u09C7\
  ."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কিভাবে:
জাভা `String.format()` এন্টারপোলেশনের জন্য প্রবর্তন করেছে:

```java
public class StringInterpolationExample {
  public static void main(String[] args) {
    String user = "Alice";
    int points = 1337;
    String greeting = String.format("হাই, %s! আপনার পয়েন্ট আছে %d.", user, points);
    System.out.println(greeting);
  }
}
```
নমুনা আউটপুট:
```
হাই, Alice! আপনার পয়েন্ট আছে 1337.
```

জাভা 15 থেকে আরও আধুনিক এন্টারপোলেশনের জন্য, আমরা টেক্সট ব্লকস এবং `formatted()` ব্যবহার করি:

```java
public class ModernStringInterpolationExample {
  public static void main(String[] args) {
    String user = "Bob";
    double accountBalance = 1234.56;
    String message = """
      প্রিয় %s,
      আপনার বর্তমান ব্যালেন্স হল $%.2f.
      """.formatted(user, accountBalance);
    System.out.println(message);
  }
}
```
নমুনা আউটপুট:
```
প্রিয় Bob,
আপনার বর্তমান ব্যালেন্স হল $1234.56.
```

## গভীর ডাইভ
এন্টারপোলেশনের আগে, জাভা `String greeting = "হ্যালো, " + user + "!";` এর মতো কনক্যাটেনেশনে নির্ভর করত। বিশেষ করে স্ট্রিং যখন জটিল হয় তখন এটি ক্লান্তিকর ও ত্রুটি প্রবণ হয়ে যায়।

ঐতিহাসিকভাবে, পার্ল এবং পিএইচপি এর মতো ভাষাগুলিতে এন্টারপোলেশন ছিল। জাভা অনেক পরে এতে যোগ দেয়। `String.format()` এবং `PrintStream.printf()` এর মতো কার্যকারিতা তাদের ফরম্যাট স্পেসিফায়ার ব্যবহার করে যা জাভাকে নির্দেশ দেয় কিভাবে ভেরিয়েবলগুলিকে হ্যান্ডেল করা যায়।

বিকল্প? `String.format()` ছাড়া, আমাদের `MessageFormat` এবং `StringBuilder` আছে, কিন্তু মৌলিক এন্টারপোলেশনের জন্য এগুলি ততো মসৃণ নয়। জাভা 15 থেকে, টেক্সট ব্লকস বহু-লাইন স্ট্রিংকে সহজ করে তোলে এবং সরাসরি জায়গায় এন্টারপোলেশন স্ট্রিমলাইন করার জন্য `formatted()` যোগ করে।

বাস্তবায়নের দিক থেকে, `String.format()` `Formatter` ব্যবহার করে, যা অনেক ফরম্যাটিং বিকল্প সহ একটি দৃঢ় ইঞ্জিন। তবে সাবধান, জটিল স্ট্রিংয়ের কারণে আপনার অ্যাপ্লিকেশনের পারফরম্যান্স ড্রপ হতে পারে যদি আপনি সতর্ক না হন।

## আরও দেখুন
- [স্ট্রিং (জাভা প্ল্যাটফর্ম এসই 8)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [ফরম্যাটার (জাভা প্ল্যাটফর্ম এসই 8)](https://docs.oracle.com/javase/8/docs/api/java/util/Formatter.html)
- [জেইপি 378: টেক্সট ব্লকস (চূড়ান্ত)](https://openjdk.java.net/jeps/378)
