---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:27.805792-06:00
description: "Kotlin-\u098F \u0995\u09CB\u09A8\u09CB \u09A1\u09BF\u09B0\u09C7\u0995\
  \u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7 \u0995\u09BF\u09A8\u09BE \u09AF\u09BE\
  \u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 \u09B9\u09B2\
  \ \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09AA\u09A5\u09C7 \u0995\
  \u09CB\u09A8\u09CB \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0\
  \ \u0989\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u09A8\u09BF\u09B6\u09CD\u099A\
  \u09BF\u09A4 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u0987 \u0995\u09BE\u099C\u099F\u09BF\
  \ \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u09A4\u09CD\u09B0\u09C1\u099F\
  \u09BF \u09AA\u09CD\u09B0\u09A4\u09BF\u09B0\u09CB\u09A7\u2026"
lastmod: '2024-03-17T18:47:44.008842-06:00'
model: gpt-4-0125-preview
summary: "Kotlin-\u098F \u0995\u09CB\u09A8\u09CB \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\
  \u099F\u09B0\u09BF \u0986\u099B\u09C7 \u0995\u09BF\u09A8\u09BE \u09AF\u09BE\u099A\
  \u09BE\u0987 \u0995\u09B0\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 \u09B9\u09B2 \u09A8\
  \u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09AA\u09A5\u09C7 \u0995\u09CB\
  \u09A8\u09CB \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u0989\
  \u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u0987 \u0995\u09BE\u099C\u099F\u09BF \u0995\u09B0\
  \u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09AA\
  \u09CD\u09B0\u09A4\u09BF\u09B0\u09CB\u09A7\u2026"
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
Kotlin-এ কোনো ডিরেক্টরি আছে কিনা যাচাই করার অর্থ হল নির্দিষ্ট পথে কোনো ডিরেক্টরির উপস্থিতি নিশ্চিত করা। প্রোগ্রামাররা এই কাজটি করে থাকেন ত্রুটি প্রতিরোধ করার জন্য, যেমন অস্তিত্বহীন কোনো ডিরেক্টরিতে পড়া বা লেখার চেষ্টা করা, যাতে অ্যাপ্লিকেশনের মধ্যে ফাইল হ্যান্ডলিং এবং ডাটা ম্যানেজমেন্ট আরও নিখুঁত হয়।

## কিভাবে:
Kotlin, JVM-এ চলছে, ফাইল অপারেশনের জন্য Java File API ব্যবহার করে, যা ডিরেক্টরির অস্তিত্ব যাচাই করা সরল করে তোলে। এখানে একটি মৌলিক উদাহরণ দেওয়া হল:

```kotlin
import java.io.File

fun main() {
    val path = "/path/to/directory"
    val directory = File(path)

    if (directory.exists() && directory.isDirectory) {
        println("Directory exists: $path")
    } else {
        println("Directory does not exist: $path")
    }
}
```
ধরে নেওয়া হল ডিরেক্টরি আছে, নমুনা আউটপুট:
```
Directory exists: /path/to/directory
```
এবং যদি না থাকে:
```
Directory does not exist: /path/to/directory
```

একটি Kotlin প্রকল্পে, আপনি সম্ভবত Kotlin-বিশেষ লাইব্রেরি বা ফ্রেমওয়ার্কের সাথে নিয়মিত কাজ করতে পারেন, যেমন ওয়েব অ্যাপ্লিকেশনের জন্য Ktor বা অ্যাসিঙ্ক্রোনাস প্রোগ্রামিংয়ের জন্য kotlinx.coroutines। তবে, ডিরেক্টরির অস্তিত্ব যাচাই করার জন্য, দেখানো মানক Java `File` API সাধারণত যথেষ্ট এবং Kotlin-এর Java এর সাথে ইন্টারঅপারেবিলিটির কারণে প্রশস্তভাবে ব্যবহৃত হয়। এই বিশেষ কাজের জন্য কোনো তৃতীয়-পক্ষের লাইব্রেরির প্রয়োজন হয় না, যা অন্য প্রোগ্রামিং ভাষা থেকে Kotlin-এ সম্প্রবর্তী শুরুকারীদের জন্য এটি সুলভ ও সরল করে তোলে।
