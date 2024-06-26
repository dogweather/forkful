---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:40:16.766162-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: Kotlin \u09AB\u09BE\u0987\u09B2\
  \u09C7 \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\
  \u09BF \u09B8\u09B9\u099C \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09AA\u09CD\u09B0\
  \u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u09A4\
  \u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\
  \u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\
  \u099C\u09A8 \u099B\u09BE\u09A1\u09BC\u09BE\u0987 \u09B8\u09CD\u099F\u09CD\u09AF\
  \u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF\u0995\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE \u09B9\u09AF\u09BC\u0964\u2026"
lastmod: '2024-03-17T18:47:44.013048-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u09AA\u09A6\u09CD\
  \u09A7\u09A4\u09BF \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, \u09AF\
  \u09C7\u0996\u09BE\u09A8\u09C7 \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\
  \u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0\
  \ \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u099B\u09BE\u09A1\u09BC\u09BE\
  \u0987 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0995\u09C7 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কীভাবে:
Kotlin ফাইলে লেখার জন্য একটি সহজ পদ্ধতি প্রদান করে, যেখানে তৃতীয় পক্ষের লাইব্রেরির প্রয়োজন ছাড়াই স্ট্যান্ডার্ড লাইব্রেরিকে ব্যবহার করা হয়। এখানে একটি সহজ উদাহরণ দেওয়া হল:

```kotlin
import java.io.File

fun main() {
    val textToWrite = "Hello, Kotlin file writing!"
    File("example.txt").writeText(textToWrite)
}
```
এই কোড স্নিপেট প্রকল্পের রুট ডিরেক্টরিতে "example.txt" নামে একটি ফাইল তৈরি করে এবং এতে `Hello, Kotlin file writing!` স্ট্রিংটি লেখে। যদি ফাইলটি আগে থেকেই বিদ্যমান থাকে, তাহলে এটি পুনরায় লেখা হবে।

একটি ফাইলে আরও নিয়ন্ত্রণের সাথে যোগ করার জন্য বা বড় পরিমাণ ডেটা লেখার জন্য, আপনি `appendText` বা `bufferedWriter()` ব্যবহার করতে পারেন:

```kotlin
import java.io.File

fun appendToFile() {
    val moreText = "Appending more text."
    File("example.txt").appendText(moreText)
}

fun writeWithBufferedWriter() {
    val largeText = "Large amounts of text...\nOn multiple lines."
    File("output.txt").bufferedWriter().use { out ->
        out.write(largeText)
    }
}

fun main() {
    appendToFile() // বিদ্যমান ফাইলে টেক্সট যোগ করে
    writeWithBufferedWriter() // বড় টেক্সট ডেটা কার্যকরভাবে লেখে
}
```

`appendToFile` ফাংশনে, আমরা "example.txt" এ আরও টেক্সট যোগ করছি যাতে এর বর্তমান কন্টেন্ট ওভাররাইট না হয়। `writeWithBufferedWriter` ফাংশনটি বড় পরিমাণে টেক্সট বা ডেটা লেখার একটি কার্যকর উপায় তুলে ধরে, বিশেষ করে যখন এটি মাল্টিপল লাইন বা বড় ফাইলের সাথে ডিল করতে হয়, তখন I/O অপারেশনগুলি মিনিমাইজ করার জন্য উপযোগী।

এই উদাহরণগুলি Kotlin এ টেক্সট ফাইল লেখার জন্য বেসিক অপারেশনগুলি কভার করে, যা Kotlin এর স্ট্যান্ডার্ড লাইব্রেরির সাহায্যে ফাইল I/O অপারেশনের সহজতা এবং ক্ষমতা প্রদর্শন করে।
