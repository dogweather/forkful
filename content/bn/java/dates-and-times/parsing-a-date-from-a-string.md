---
title:                "স্ট্রিং থেকে তারিখ পার্স করা"
date:                  2024-03-17T18:05:44.161474-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
একটি স্ট্রিং থেকে তারিখ পার্সিং করা মানে তারিখ ও সময়ের টেক্সট প্রতিনিধিত্বকে `Date` অবজেক্ট বা আরও আধুনিক `LocalDateTime` অবজেক্টে রূপান্তর করা। প্রোগ্রামাররা তারিখ নিয়ন্ত্রণ, ফরম্যাট, তুলনা বা একটি মানকৃত ফর্ম্যাটে তারিখ সংরক্ষণ করার জন্য এটি করে থাকেন, যা তারিখের গণনা, যাচাইকরণ বা ধারাবাহিক আন্তর্জাতিকীকরণের দাবি করে এমন অ্যাপ্লিকেশনের জন্য অপরিহার্য।

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
