---
title:                "টেক্সট ফাইল পড়া"
date:                  2024-03-17T18:08:55.196196-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

টেক্সট ফাইল পড়া মানে হচ্ছে আপনার প্রোগ্রামের মধ্যে একটি ফাইল থেকে ডাটা টানা, সাধারণত লাইন বাই লাইন। প্রোগ্রামাররা এটি করেন বাহ্যিকভাবে সংরক্ষিত ডাটা প্রক্রিয়া করার অথবা বিশ্লেষণ করার জন্য।

## কিভাবে:

Kotlin এ, আপনি সহজেই একটি টেক্সট ফাইল পড়তে পারবেন `readLines()` ফাংশন বা `useLines` ব্লক ব্যবহার করে।

```Kotlin
import java.io.File

fun main() {
    // একবারে সব লাইন পড়ুন
    val lines = File("example.txt").readLines()
    lines.forEach { line ->
        println(line)
    }

    // বড় ফাইলের জন্য আরো কার্যকর
    File("example.txt").useLines { lines ->
        lines.forEach { line ->
            println(line)
        }
    }
}
```

নমুনা আউটপুট (ধরুন `example.txt` দুই লাইন থাকে "Hello" এবং "World" সহ):

```
Hello
World
```

## গভীরে ডুব:

ঐতিহাসিকভাবে, Java তে ফাইল পড়া হতে পারে বাক্বপূর্ন এবং ক্লাঙ্কি। Kotlin এ, স্ট্যান্ডার্ড লাইব্রেরি ফাইল পড়াকে সহজ করার জন্য সুবিধাজনক এক্সটেনশন প্রদান করে।

Kotlin এ ফাইল পড়ার বিকল্পগুলি:
১. `readText()` পুরো ফাইলের সামগ্রীকে একটি `String` এ পড়ে।
২. `bufferedReader()` একটি `BufferedReader` প্রদান করে যা আপনাকে আরও জটিল ব্যবহারের মামলাগুলি, যেমন বিশাল ফাইলগুলি পড়ার সময় প্রচুর মেমরি ব্যবহার না করে সামলাতে দেয়।

বাস্তবায়নের দিক থেকে, `useLines` ব্যবহার করার সময়, এটি কার্যনির্বাহের পর ফাইল বন্ধ করার দায়িত্ব নেয়, সম্ভাব্য মেমরি ফাঁস রোধ করে। এটি একটি কর্মাত্মক দৃষ্টিকোণ যা Kotlin এ কার্যকরভাবে সম্পদ পরিচালনার জন্য উৎসাহিত করা হয়।

## আরও দেখুন

- ফাইল পড়া সম্পর্কে Kotlin ডকুমেন্টেশন: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- আরও জটিল ক্ষেত্রের জন্য `BufferedReader` ডকুমেন্টেশন: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/)
