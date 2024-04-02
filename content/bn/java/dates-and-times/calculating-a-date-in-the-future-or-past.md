---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:32.593180-06:00
description: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4\u09C7 \u0985\u09A5\u09AC\u09BE\
  \ \u0985\u09A4\u09C0\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\
  \u0995\u099F\u09BF \u099C\u09BE\u09A8\u09BE \u09A4\u09BE\u09B0\u09BF\u0996\u0995\
  \u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u0982\u0996\
  \u09CD\u09AF\u09BE\u09B0 \u09A6\u09BF\u09A8, \u09AE\u09BE\u09B8 \u09AC\u09BE \u09AC\
  \u099B\u09B0 \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u09B8\u09AE\u09A8\u09CD\u09AC\
  \u09AF\u09BC \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09A4\u09BF\u09A8\u09BF\
  \u09A7\u09BF\u0997\u09A3 \u098F\u099F\u09BF \u09AE\u09A8\u09C7\u09B0\u09BE\u0996\
  \u09BE\u09B0 \u09B8\u09CD\u09AE\u09BE\u09B0\u0995,\u2026"
lastmod: '2024-03-17T18:47:43.918273-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4\u09C7 \u0985\u09A5\u09AC\u09BE\
  \ \u0985\u09A4\u09C0\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\
  \u0995\u099F\u09BF \u099C\u09BE\u09A8\u09BE \u09A4\u09BE\u09B0\u09BF\u0996\u0995\
  \u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u0982\u0996\
  \u09CD\u09AF\u09BE\u09B0 \u09A6\u09BF\u09A8, \u09AE\u09BE\u09B8 \u09AC\u09BE \u09AC\
  \u099B\u09B0 \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u09B8\u09AE\u09A8\u09CD\u09AC\
  \u09AF\u09BC \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09A4\u09BF\u09A8\u09BF\
  \u09A7\u09BF\u0997\u09A3 \u098F\u099F\u09BF \u09AE\u09A8\u09C7\u09B0\u09BE\u0996\
  \u09BE\u09B0 \u09B8\u09CD\u09AE\u09BE\u09B0\u0995,\u2026"
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কি এবং কেন?

ভবিষ্যতে অথবা অতীতে একটি তারিখ গণনা করা মানে একটি জানা তারিখকে নির্দিষ্ট সংখ্যার দিন, মাস বা বছর দ্বারা সমন্বয় করা। প্রতিনিধিগণ এটি মনেরাখার স্মারক, মেয়াদ উত্তীর্ণ তারিখ, এবং ইভেন্টস সিডিউলিং এর মত ফিচারগুলির জন্য করে থাকে।

## কিভাবে:

```java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class DateCalculation {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        // বর্তমান তারিখে 10 দিন যোগ করুন
        LocalDate futureDate = today.plusDays(10);
        System.out.println("ভবিষ্যত তারিখ: " + futureDate);

        // বর্তমান তারিখ থেকে 2 মাস বাদ দিন
        LocalDate pastDate = today.minus(2, ChronoUnit.MONTHS);
        System.out.println("অতীত তারিখ: " + pastDate);
    }
}
```

আউটপুট এরকম দেখাবে:

```
ভবিষ্যত তারিখ: 2023-04-30
অতীত তারিখ: 2023-02-20
```

## গভীর ভাবনা

জাভা 8 আসার আগে, তারিখ নিয়ে কাজ করা কঠিন ছিল। পুরানো ক্লাস সমূহ যেমন `java.util.Date` এবং `java.util.Calendar` বাগে ভরা এবং ব্যবহারে অসুবিধাজনক ছিল। জাভা 8-তে চালু করা `java.time` প্যাকেজটি এই সমস্যা দূর করেছে, `LocalDate`, `LocalTime`, এবং `ZonedDateTime` এর মত ভালোভাবে সাজানো ক্লাসগুলির মাধ্যমে।

বিকল্প? জাভা 8 এর পূর্বে, জোডা-টাইমের মতো থার্ড-পার্টি লাইব্রেরিগুলি সাধারণ ছিল। আজকাল, আপনি এগুলি ব্যবহার করতে পারেন, তবে স্ট্যান্ডার্ড `java.time` ব্যবহার করা পরামর্শ দেওয়া হয় কারণ এটি জাভার একটি অফিসিয়াল অংশ এবং এটি দিনের আলো সঞ্চয়, সময় অঞ্চল, এবং লিপ বছরগুলিকে সুন্দরভাবে সামলায়।

তারিখের হিসাবনিকাশ কোডিং করার সময়, আপনার প্রয়োজনে সময় অঞ্চল বিবেচনা করুন। UTC এর জন্য, `LocalDate` এর পরিবর্তে `Instant` ব্যবহার করুন। নির্দিষ্ট অঞ্চলের জন্য, সাধারণত `ZonedDateTime` ব্যবহার করা হয়। মনে রাখবেন, তারিখ-সময়ের অপারেশনগুলি চেইন করা যায়, যেমন `date.minusWeeks(1).plusHours(3)` এর মতো, যা আপনার কোডকে আরও পরিষ্কার করে তোলে।

## দেখুন এছাড়াও

1. `java.time` প্যাকেজ অভিধান: [Oracle Docs](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
2. `ZonedDateTime` এর সাথে সময় অঞ্চল সামলানো: [Oracle ZonedDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/ZonedDateTime.html)
3. `java.time.format.DateTimeFormatter` এর জন্য অফিসিয়াল তারিখ এবং সময় প্যাটার্নস: [Oracle DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
