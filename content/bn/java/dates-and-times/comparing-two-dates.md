---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:34.104370-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\u09BE \u09AF\
  \u09A5\u09C7\u09B7\u09CD\u099F \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A6\u09C7\
  \u09AF\u09BC\u0964 `LocalDate` \u098F\u09AC\u0982 `compareTo`, `isBefore`, \u0985\
  \u09A5\u09AC\u09BE `isAfter` \u09AE\u09C7\u09A5\u09A1 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C1\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0995\
  \u09BF\u099B\u09C1 \u09A4\u09A5\u09CD\u09AF."
lastmod: '2024-03-17T18:47:43.917299-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE \u09AF\u09A5\u09C7\u09B7\u09CD\u099F \u09B8\u09B9\
  \u099C \u0995\u09B0\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 `LocalDate` \u098F\u09AC\
  \u0982 `compareTo`, `isBefore`, \u0985\u09A5\u09AC\u09BE `isAfter` \u09AE\u09C7\u09A5\
  \u09A1 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8\u0964\
  \ \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09BF\u099B\u09C1 \u09A4\u09A5\u09CD\u09AF\
  ."
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কিভাবে:
জাভা তারিখ তুলনা করা যথেষ্ট সহজ করে দেয়। `LocalDate` এবং `compareTo`, `isBefore`, অথবা `isAfter` মেথড ব্যবহার করুন। এখানে কিছু তথ্য:

```java
import java.time.LocalDate;

public class DateComparison {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2023, 4, 1);
        LocalDate date2 = LocalDate.now(); // ধরা যাক আজকের তারিখ 2023-4-15

        // compareTo ব্যবহার করে
        int comparisonResult = date1.compareTo(date2);
        if(comparisonResult < 0) {
            System.out.println("Date1 is before Date2");
        } else if (comparisonResult > 0) {
            System.out.println("Date1 is after Date2");
        } else {
            System.out.println("Date1 is same as Date2");
        }

        // isBefore এবং isAfter ব্যবহার করে
        if(date1.isBefore(date2)) {
            System.out.println("Date1 is earlier than Date2");
        } else if(date1.isAfter(date2)) {
            System.out.println("Date1 is later than Date2");
        } else {
            System.out.println("Date1 is the same day as Date2");
        }
    }
}
```

আজকের তারিখের জন্য নমুনা আউটপুট হচ্ছে 2023-04-15:

```
Date1 is before Date2
Date1 is earlier than Date2
```

## গভীরে গমন
ঐতিহাসিকভাবে, জাভার তারিখ হ্যান্ডলিং ছিল, না, একটি মাথাব্যথা। কিন্তু তারপর এসেছে জাভা 8 সহ `java.time`, একটি গেম চেঞ্জার। এখন আমরা `LocalDate` ব্যবহার করি সময় ছাড়াই তারিখের জন্য। সময় সহ তারিখ তুলনা করতে চান? `LocalDateTime`-এর দিকে তাকান।

বিকল্প? অবশ্যই। জাভা 8 এর আগে, ছিল `java.util.Date` এবং `java.util.Calendar`। আপনি এগুলি এখনও ব্যবহার করতে পারেন, কিন্তু নিজের কবর খনন করতে চাইবেন কেন?

বাস্তবায়নের দিক থেকে, `compareTo` `int` ফেরত দেয়: নেগেটিভ যদি কলিং অবজেক্ট কম (আগে), শূন্য যদি সমান, পজিটিভ যদি বেশি (পরে)। `isBefore` এবং `isAfter` `boolean` ফেরত দেয়। বুঝতে সহজ, এবং কোনো দুর্বোধ্য নেই।

## আরও দেখুন
আরও বিস্তারিত জানার জন্য এগুলিতে ডুব দিন:

- [অরাকলের জাভা ডকুমেন্টেশনে LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [অরাকলের ডেট টাইমের উপর টিউটোরিয়াল](https://docs.oracle.com/javase/tutorial/datetime/)
- বাস্তব ব্যবহার এবং সমস্যা সমাধানের জন্য স্ট্যাক ওভারফ্লো:
  - [`LocalDate` ব্যবহার করে](https://stackoverflow.com/questions/tagged/localdate)
  - [জাভা ডেট বনাম ক্যালেন্ডার](https://stackoverflow.com/questions/5369682/get-current-time-and-date-on-android)
