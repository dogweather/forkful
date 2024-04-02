---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:11:54.612138-06:00
description: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982\
  \ \u09B9\u099A\u09CD\u099B\u09C7 \u09AC\u09BF\u09A6\u09CD\u09AF\u09AE\u09BE\u09A8\
  \ \u0995\u09CB\u09A1\u09C7 \u099F\u09C1\u0987\u0995\u09BF\u0982 \u0995\u09B0\u09BE\
  \u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u09AF\u09BE\
  \ \u098F\u09B0 \u0997\u09A0\u09A8, \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF\
  \u09A4\u09BE \u098F\u09AC\u0982 \u09AA\u09BE\u09B0\u09AB\u09B0\u09AE\u09C7\u09A8\
  \u09CD\u09B8 \u0989\u09A8\u09CD\u09A8\u09A4\u09BF \u0995\u09B0\u09C7 \u0995\u09BF\
  \u09A8\u09CD\u09A4\u09C1 \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u0986\u099A\
  \u09B0\u09A3 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09C7\
  \ \u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.002777-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982\
  \ \u09B9\u099A\u09CD\u099B\u09C7 \u09AC\u09BF\u09A6\u09CD\u09AF\u09AE\u09BE\u09A8\
  \ \u0995\u09CB\u09A1\u09C7 \u099F\u09C1\u0987\u0995\u09BF\u0982 \u0995\u09B0\u09BE\
  \u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u09AF\u09BE\
  \ \u098F\u09B0 \u0997\u09A0\u09A8, \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF\
  \u09A4\u09BE \u098F\u09AC\u0982 \u09AA\u09BE\u09B0\u09AB\u09B0\u09AE\u09C7\u09A8\
  \u09CD\u09B8 \u0989\u09A8\u09CD\u09A8\u09A4\u09BF \u0995\u09B0\u09C7 \u0995\u09BF\
  \u09A8\u09CD\u09A4\u09C1 \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u0986\u099A\
  \u09B0\u09A3 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09C7\
  \ \u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE\u2026"
title: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982"
weight: 19
---

## কি এবং কেন?
রিফ্যাক্টরিং হচ্ছে বিদ্যমান কোডে টুইকিং করার প্রক্রিয়া যা এর গঠন, পাঠযোগ্যতা এবং পারফরমেন্স উন্নতি করে কিন্তু বাহ্যিক আচরণ পরিবর্তন করে না। প্রোগ্রামাররা কোডকে আরও রক্ষণশীল, নতুন বৈশিষ্ট্য যোগ করা সহজ করার জন্য, এবং বাগ সহজে চিহ্নিত করে ঠিক করার জন্য রিফ্যাক্টর করে থাকেন।

## কিভাবে:
এখানে একটি কমন কোডের দুর্গন্ধ এবং এর রিফ্যাক্টর করা সংস্করণ দেখানো হলো কোটলিন স্নিপেটে। আমরা এমন একটি কোড ব্লক দিয়ে শুরু করি যা অধিকতর কাজ করছে:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Order ID: ${order.id}")
        // অর্ডার টোটাল গণনা করা
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // ডিসকাউন্ট প্রয়োগ
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Total: $total")
        // আরও প্রক্রিয়াজাতিকরণ...
    }
}
```

উন্নত পাঠযোগ্যতা এবং সংক্রান্তিকরণের জন্য রিফ্যাক্টর করা হয়েছে:

```kotlin
fun printOrderSummary(order: Order) {
    print("Order ID: ${order.id}")
    val total = calculateTotal(order)
    print("Total: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

এখানে কার্যকারিতা পরিবর্তন হয়নি বলে কোন নমুনা আউটপুট নেই, তবে কোডের পাঠযোগ্যতা এবং রক্ষণাবেক্ষণ ব্যাপকভাবে বাড়িয়েছে!

## গভীর ডুব
রিফ্যাক্টরিং এর ধারণা প্রোগ্রামিং শুরু হওয়া থেকেই ছিল, তবে ১৯৯০ এর দশকে এটি একটি শৃঙ্খলাবদ্ধ পদ্ধতি হিসেবে গতি পায়, বিশেষ করে মার্টিন ফাওলার ১৯৯৯ সালে "রিফ্যাক্টরিং: বিদ্যমান কোডের নকশা উন্নতি" প্রকাশ করার পর। এই বইটি এই অনুশীলনকে একটি নাম দিয়েছে এবং এর প্রয়োগের জন্য একটি শৃঙ্খলাবদ্ধ পদ্ধতি নির্ধারণ করেছে, রিফ্যাক্টরিং কৌশলের একটি ক্যাটালগ সহ।

রিফ্যাক্টরিংকে বিকল্পগুলির সাথে তুলনা: আপনি কোড খসড়া থেকে নতুন করে লেখা শুরু করতে পারেন (ঝুঁকিপূর্ণ এবং সময় সাপেক্ষ), অথবা কেবল যোগফলের পরিবর্তন করতে পারেন (এটি সফটওয়্যার ব্রাট এবং সম্ভাব্য টেক ঋণে পরিণত করতে পারে)। রিফ্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�্�RowAnimation wise, it's essential to have a robust set of tests before you start refactoring to ensure you don't accidentally change the program's behavior. Many modern IDEs (including IntelliJ for Kotlin) have automated refactoring tools to rename variables, extract methods, and more, which can speed up the process and reduce errors.

## আরও দেখুন
- "Refactoring: Improving the Design of Existing Code" মার্টিন ফাওলার দ্বারা (এই বিষয়ের উপর মৌলিক কাজের জন্য)
- ক্লিন কোড 'কোটলিন উপায়' বুঝতে কোটলিন ডকুমেন্টেশনের কোডিং কনভেনশনস: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html)
- IntelliJ IDEA তে রিফ্যাক্টরিং সমর্থনের জন্য JetBrains সাপোর্ট: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (বাস্তব রিফ্যাক্টরিং টুল ব্যবহারের জন্য)
- বৃহত্তর রিফ্যাক্টরিং চ্যালেঞ্জগুলি সমাধানে Google-এর গাইড: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (বৃহত্তর রিফ্যাক্টরিং চ্যালেঞ্জগুলির দৃষ্টিভঙ্গিতে)
