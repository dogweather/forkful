---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:40.880812-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE \u09AC\
  \u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09C7\
  \u09A4\u09C7 \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u0989\u09AA\u09BE\u09AF\
  \u09BC \u0989\u09AA\u09B8\u09CD\u09A5\u09BE\u09AA\u09A8 \u0995\u09B0\u09C7, \u09AA\
  \u09C1\u09B0\u09BE\u09A8\u09CB `java.util.Date` \u0995\u09CD\u09B2\u09BE\u09B8 \u098F\
  \u09AC\u0982 \u0986\u09B0\u0993 \u09AC\u09BF\u09B6\u09C7\u09B7\u09A4 \u098F\u09AC\
  \u0982 \u09B8\u09B9\u099C\u09AC\u09CB\u09A7\u09CD\u09AF \u09A8\u09A4\u09C1\u09A8\
  \ `java.time` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C (\u099C\u09BE\u09AD\u09BE\
  \ \u09EE-\u098F\u2026"
lastmod: '2024-03-17T18:47:43.915251-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 \u09AC\u09BF\u09AD\u09BF\u09A8\
  \u09CD\u09A8 \u0989\u09AA\u09BE\u09AF\u09BC \u0989\u09AA\u09B8\u09CD\u09A5\u09BE\
  \u09AA\u09A8 \u0995\u09B0\u09C7, \u09AA\u09C1\u09B0\u09BE\u09A8\u09CB `java.util.Date`\
  \ \u0995\u09CD\u09B2\u09BE\u09B8 \u098F\u09AC\u0982 \u0986\u09B0\u0993 \u09AC\u09BF\
  \u09B6\u09C7\u09B7\u09A4 \u098F\u09AC\u0982 \u09B8\u09B9\u099C\u09AC\u09CB\u09A7\
  \u09CD\u09AF \u09A8\u09A4\u09C1\u09A8 `java.time` \u09AA\u09CD\u09AF\u09BE\u0995\
  \u09C7\u099C (\u099C\u09BE\u09AD\u09BE \u09EE-\u098F \u09AA\u09CD\u09B0\u09AC\u09B0\
  \u09CD\u09A4\u09BF\u09A4) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7\u0964\n\n#."
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কীভাবে:
জাভা বর্তমান তারিখ পেতে বিভিন্ন উপায় উপস্থাপন করে, পুরানো `java.util.Date` ক্লাস এবং আরও বিশেষত এবং সহজবোধ্য নতুন `java.time` প্যাকেজ (জাভা ৮-এ প্রবর্তিত) ব্যবহার করে।

### `java.time.LocalDate` ব্যবহার করে
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // উদাহরণ আউটপুট: 2023-04-01
    }
}
```

### `java.time.LocalDateTime` ব্যবহার করে
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // উদাহরণ আউটপুট: 2023-04-01T12:34:56.789
    }
}
```

### `java.util.Date` ব্যবহার করে (পুরাতন)
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // উদাহরণ আউটপুট: শনি এপ্রিল 01 12:34:56 BST 2023
    }
}
```

### কোনো তৃতীয়-পক্ষীয় লাইব্রেরি ব্যবহার: জোডা-টাইম
জাভা ৮-এর আগে, জোডা-টাইম ছিল জাভায় তারিখ এবং সময়ের জন্য মানদণ্ড। যদি আপনি পুরানো সিস্টেমে কাজ করছেন অথবা জোডা-টাইমের প্রতি আগ্রহী, তবে এইভাবে আপনি বর্তমান তারিখ পেতে পারেন:
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // উদাহরণ আউটপুট: 2023-04-01
    }
}
```
**লক্ষ্য করুন:** যদিও `java.util.Date` এবং জোডা-টাইম এখনও ব্যবহৃত হয়, নতুন প্রজেক্টগুলির জন্য অচলতা এবং তারিখ এবং সময় হ্যান্ডল করার জন্য সম্পূর্ণ API এর কারণে `java.time` প্যাকেজ সুপারিশ করা হয়।
