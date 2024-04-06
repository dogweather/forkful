---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:05:44.161474-06:00
description: null
lastmod: '2024-04-05T21:53:52.173549-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কিভাবে:


### `java.time` প্যাকেজ ব্যবহার করে (জাভা 8 এবং পরবর্তীতে প্রস্তাবিত):
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // আউটপুট: 2023-04-30
    }
}
```

### `SimpleDateFormat` ব্যবহার করে (পুরোনো পদ্ধতি):
```java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "30/04/2023";
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date date = formatter.parse(dateString);
            System.out.println(date); // আউটপুটের ফরম্যাট আপনার সিস্টেমের ডিফল্ট ফরম্যাটের উপর নির্ভর করে
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### থার্ড-পার্টি লাইব্রেরি ব্যবহার করে (যেমন, Joda-Time):
Joda-Time একটি উল্লেখযোগ্য থার্ড-পার্টি লাইব্রেরি ছিল, কিন্তু জাভা 8-এ `java.time` প্যাকেজের প্রবর্তনের কারণে এটি এখন রক্ষণাবেক্ষণ মোডে আছে। তবে, জাভা 8-এর আগের সংস্করণ ব্যবহার করা জন্য Joda-Time একটি ভাল পছন্দ।
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // আউটপুট: 2023-04-30
    }
}
```
মনে রাখবেন যে তারিখের সাথে কাজ করার সময়, শুধুমাত্র তারিখ নয়, তারিখ-সময় পার্স বা ফরম্যাট করার ক্ষেত্রে সময় অঞ্চলের সেটিংসের দিকে সবসময় সচেতন থাকুন।
