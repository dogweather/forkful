---
title:                "বর্তমান তারিখ পেতে"
date:                  2024-03-17T17:48:40.880812-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
জাভায় বর্তমান তারিখ পাওয়া একটি মূল অপারেশন, যা প্রোগ্রামারদের লগিং, তারিখের হিসাব-নিকাশ, এবং সময়-ভিত্তিক শর্তাবলীর মতো অপারেশনের জন্য তারিখের অবজেক্ট ম্যানিপুলেট করতে সক্ষম করে। যেখানে ট্র্যাকিং, শিডিউলিং, এবং সময়সংক্রান্ত ডেটা বিশ্লেষণ অপরিহার্য, সেখানে এই অপারেশনটি জরুরি।

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
