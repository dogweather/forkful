---
title:                "তারিখকে স্ট্রিং এ রূপান্তর করা"
date:                  2024-03-17T17:46:36.628109-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

একটি তারিখকে স্ট্রিংয়ে পরিণত করা মানে একটি তারিখ অবজেক্টকে পঠনযোগ্য টেক্সট হিসেবে উপস্থাপন করা যা একটি নির্দিষ্ট প্যাটার্ন অনুসরণ করে। প্রোগ্রামাররা এটি ব্যবহারকারীদের কাছে তারিখগুলি প্রদর্শন করার জন্য অথবা তাদের মানব-বান্ধব ফরম্যাটে সংরক্ষণ ও নেটওয়ার্কিংয়ের জন্য সিরিয়ালাইজ করার জন্য করে থাকেন।

## কিভাবে:

জাভা তারিখ থেকে স্ট্রিং-এ পরিবর্তনকে সরল করে তোলে। `java.time.format.DateTimeFormatter` ক্লাস হল আপনার যাবতীয় সমাধান। এখানে একটি কোড উদাহরণ দেওয়া হল:

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateToStringExample {
    public static void main(String[] args) {
        LocalDate date = LocalDate.now(); // আজকের তারিখ
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        String dateString = date.format(formatter);
        System.out.println(dateString); // আউটপুট হতে পারে: 20/03/2023, উদাহরণস্বরূপ
    }
}
```

## গভীর ডুব

ঐতিহাসিকভাবে, জাভা `java.text` প্যাকেজ থেকে `SimpleDateFormat` ব্যবহার করত, কিন্তু এটি ছিল থ্রেড-সেফ নয় এবং বাগের জন্য অগ্রসর হত। জাভা 8-এ, `java.time` প্যাকেজ থ্রেড-সেফ এবং অপরিবর্তনীয় তারিখ-সময় ক্লাসগুলি নিয়ে এসেছে। `DateTimeFormatter` হল এই আধুনিক প্যাকেজের একটি অংশ।

Apache Commons থেকে `FastDateFormat` এবং বিভিন্ন লাইব্রেরি থেকে `DateUtils` এর মতো বিকল্পগুলি রয়েছে। তবে, অধিকাংশ জাভা ডেভেলপাররা মানক লাইব্রেরির সাথে থাকেন, যা শক্তিশালী এবং বহুমুখী।

ফরম্যাটিংয়ের সময়, `DateTimeFormatter` বছরের জন্য `yyyy`, মাসের জন্য `MM`, এবং দিনের জন্য `dd` প্যাটার্ন ব্যবহার করে। এর `ofPattern` পদ্ধতি দ্বারা এটি বেশ জটিল প্যাটার্ন, এমনকি স্থানীয়-বিশেষ গুলিও সামলাতে পারে। এটি উল্লেখ করা যোগ্য যে `DateTimeFormatter` অপরিবর্তনীয় এবং থ্রেড-সেফ, সুতরাং আপনি একই ফরম্যাটার ইনস্ট্যান্স বিভিন্ন থ্রেডে ব্যবহার করতে পারেন কোনো সিনক্রোনাইজেশন মাথাব্যথা ছাড়াই।

## দেখুন এছাড়াও

- Oracle-এর অফিসিয়াল জাভা ডকুমেন্টেশন `DateTimeFormatter` এর জন্য: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- আরো তারিখ এবং সময় প্যাটার্নের জন্য: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#patterns
- জাভা 8 তারিখ এবং সময় ওভারভিউ: https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html
