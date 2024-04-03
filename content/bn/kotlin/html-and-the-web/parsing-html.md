---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:04:58.370786-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Kotlin \u099C\u09B8\u09C1\u09AA\
  \u09C7\u09B0 \u09AE\u09A4\u09CB \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09BF\u0997\u09C1\u09B2\u09BF\u09B0 \u09B8\u09BE\u09A5\u09C7 HTML \u09AA\u09BE\u09B0\
  \u09CD\u09B8\u09BF\u0982 \u09B8\u09B0\u09B2 \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\
  \u09C7\u0964 \u098F\u099F\u09BE \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\
  \u09AC\u09C7\u09A8 \u09A4\u09BE \u09A8\u09BF\u099A\u09C7 \u09A6\u09C7\u0993\u09AF\
  \u09BC\u09BE \u09B9\u09B2."
lastmod: '2024-03-17T18:47:43.991080-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u099C\u09B8\u09C1\u09AA\u09C7\u09B0 \u09AE\u09A4\u09CB \u09B2\u09BE\
  \u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF\u09B0 \u09B8\u09BE\
  \u09A5\u09C7 HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u09B8\u09B0\u09B2\
  \ \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964 \u098F\u099F\u09BE \u0995\u09BF\
  \u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09AC\u09C7\u09A8 \u09A4\u09BE \u09A8\u09BF\
  \u099A\u09C7 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কিভাবে:
Kotlin জসুপের মতো লাইব্রেরিগুলির সাথে HTML পার্সিং সরল করে তোলে। এটা কিভাবে করবেন তা নিচে দেওয়া হল:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Sample Page</title></head><body><p>This is a test.</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    println("Title: $title")  // আউটপুট: Title: Sample Page

    val pText = doc.select("p").first()?.text()
    println("Paragraph: $pText")  // আউটপুট: Paragraph: This is a test.
}
```

আমরা এর শিরোনাম এবং প্যারাগ্রাফের টেক্সট ধারণ করি, যা জসুপ কি করতে পারে তার খুব ভাসা ভাসা একটি ধারণা। কিন্তু এটা একটি শুরু।

## গভীর ডুব:
Kotlin এর আগে, এই কাজের জন্য Java ছিল প্রধান নির্বাচন, প্রায় অসুবিধাজনকভাবে। Jsoup কাহিনীটি পাল্টে দিয়েছে jQuery-এর মতো দৃষ্টিভঙ্গি প্রদান করে। HTML পার্সিং শুধুমাত্র Jsoup দ্বারা সীমাবদ্ধ নয়; HtmlUnit অথবা এমনকি regex (যদিও বিরুদ্ধে পরামর্শ দেওয়া হয়) মতো অন্যান্য লাইব্রেরিও বিদ্যমান। Jsoup এর সাথে, আপনি নিশ্চিত করতে পারেন যে আপনার পার্সিং ডকুমেন্টের কাঠামো সন্মান করে। এটি একটি DOM মডেল ব্যবহার করে, যা উপাদানগুলি নির্বাচন এবং ম্যানিপুলেশন সম্ভব করে তোলে। এটি একটি দৃঢ় লাইব্রেরি, এমনকি সবচেয়ে জটিল HTML পার্স করতে সক্ষম।

## আরও দেখুন:
Jsoup সম্পর্কে গভীরে ডুব দিন:

- Jsoup অফিসিয়াল ডকুমেন্টেশন: https://jsoup.org/
- "Kotlin for Android Developers" বই: https://antonioleiva.com/kotlin-android-developers-book/
- Kotlin প্রোগ্রামিং ভাষা অফিসিয়াল সাইট: https://kotlinlang.org/

ওয়েব স্ক্র্যাপিং এবং পার্সিং সম্পর্কে ব্যাপক আলোচনা ও টিউটোরিয়ালের জন্য:

- Kotlin এবং Jsoup দিয়ে ওয়েব স্ক্র্যাপিং: https://medium.com/@hadiyarajesh/web-scraping-with-kotlin-and-jsoup-8b5b6c31c5a5
- Kotlin এবং Jsoup দিয়ে Android এ HTML পার্সিং: https://proandroiddev.com/parsing-html-on-android-1b766658be6a
