---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:05.820298-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Kotlin \u09A4\u09CD\u09B0\u09C1\
  \u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B8\u09CD\u09A5\u09BE\u09AA\u09A8\u09BE\
  \ \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF `try`, `catch`, `finally`, \u098F\
  \u09AC\u0982 `throw` \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964\
  \ \u0986\u09AA\u09A8\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u09A4\u09BE\u09A6\
  \u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u09A8\
  \ \u09A4\u09BE \u098F\u0996\u09BE\u09A8\u09C7."
lastmod: '2024-03-17T18:47:44.001770-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B8\
  \u09CD\u09A5\u09BE\u09AA\u09A8\u09BE \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF `try`, `catch`, `finally`, \u098F\u09AC\u0982 `throw` \u09AA\u09CD\u09B0\u09A6\
  \u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u0986\u09AA\u09A8\u09BF \u0995\u09BF\u09AD\
  \u09BE\u09AC\u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7\u09A8 \u09A4\u09BE \u098F\u0996\u09BE\u09A8\u09C7\
  ."
title: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u0997\u09C1\u09B2\u09BF \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE"
weight: 16
---

## কিভাবে:
Kotlin ত্রুটি ব্যবস্থাপনা করার জন্য `try`, `catch`, `finally`, এবং `throw` প্রদান করে। আপনি কিভাবে তাদের ব্যবহার করেন তা এখানে:

```Kotlin
fun main() {
    val numerator = 10
    val denominator = 0

    try {
        val result = numerator / denominator
        println("Result: $result")
    } catch (e: ArithmeticException) {
        println("শূন্য দিয়ে ভাগ করা যায় না, বন্ধু।")
    } finally {
        println("এটি যেকোন অবস্থায় ঘটবে।")
    }
}
```

আউটপুট:
```
শূন্য দিয়ে ভাগ করা যায় না, বন্ধু।
এটি যেকোন অবস্থায় ঘটবে।
```

`try` ব্লকে কিছু ভুল হলে, কার্যনির্বাহ `catch` এ চলে যায়। এটি নির্দিষ্ট ত্রুটির জন্য ধরা দেয় (`ArithmeticException` এই ক্ষেত্রে)। `finally` ব্লক পরে চালায়—ফলাফলে যা হোক না কেন।

## গভীর ডুব
`try-catch` ব্লক প্রাচীন প্রোগ্রামিং দিন থেকে একটি জিনিস—এটি একটি নিরাপত্তা জালের মতো। Kotlin ম্যানুয়ালি একটি ব্যতিক্রম ছুঁড়ে দেওয়ার জন্য `throw` এবং চালানোর জন্য কোডের জন্য `finally` অফার করে—প্রায়শই সাফাই করার কাজ।

বিকল্পগুলোর মধ্যে `Result` টাইপ এবং Kotlin এর `try` একটি প্রকাশ হিসাবে রয়েছে।

```Kotlin
val result: Result<Int> = try {
    Result.success(numerator / denominator)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
এই পদ্ধতি একটি `Result` অবজেক্ট ফেরত—আপনি একটি সাফল্য বা ব্যর্থতা পান একটি অপ্রকাশিত ব্যতিক্রমের নাটক ছাড়াই।

Kotlin এ বাস্তবায়ন দারুণ কারণ আপনি একটি প্রকাশ হিসেবে `try` ব্যবহার করতে পারেন, অর্থাৎ এটি একটি মান ফেরত দেয়। এই ধরনের পছন্দ করা Kotlin এ ত্রুটি সম্পর্কের করা বেশ বহুমুখী করে তোলে। এটি কর্মশালায় আপনি যা করতে যাবেন সেই জন্য সঠিক টুল নির্বাচনের মতো।

## আরও দেখুন
- Kotlin ত্রুটি সম্পর্কে ডকুমেন্টেশন: [Kotlin ত্রুটি ব্যবস্থাপনা](https://kotlinlang.org/docs/exception-handling.html)
- Kotlin `Result` টাইপ ডকুমেন্টেশন: [Kotlin Result](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- জোশুয়া ব্লচ রচিত ইফেক্টিভ জাভা, ৩য় সংস্করণ—ব্যতিক্রম সম্পর্কে দুর্দান্ত দৃষ্টিকোণ, যদিও এটি জাভা-বিশেষ।
