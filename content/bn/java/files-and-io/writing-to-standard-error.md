---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:43.231680-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: #."
lastmod: '2024-03-17T18:47:43.921315-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কিভাবে:


### জাভাতে মৌলিক stderr আউটপুট
জাভা `System.err.print()` বা `System.err.println()` ব্যবহার করে stderr এ লিখতে একটি সহজ উপায় প্রদান করে। এটা কিভাবে করবেন তা নিচে দেখুন:

```java
public class StdErrExample {
    public static void main(String[] args) {
        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("Error: Cannot divide by zero.");
        }
    }
}
```

নমুনা আউটপুট:

```
Error: Cannot divide by zero.
```

এটি সরাসরি মানক ত্রুটি স্ট্রিমে ত্রুটি বার্তাটি প্রিন্ট করবে।

### উন্নত ত্রুটি হ্যান্ডলিংয়ের জন্য লগার ব্যবহার
যেসব অ্যাপলিকেশনের জন্য আরও উন্নত ত্রুটি হ্যান্ডলিং এবং লগিং প্রয়োজন হয়, তাদের জন্য SLF4J এর সাথে Logback বা Log4J2 এর মতো লগিং লাইব্রেরি ব্যবহার করা সাধারণ। এটি ত্রুটি আউটপুট পরিচালনায় আরও নমনীয়তা প্রদান করে, যার অন্তর্গত হল ফাইল পুনঃনির্দেশ, ফিল্টারিং, এবং ফর্ম্যাটিং।

#### Logback এর সাথে উদাহরণ
প্রথমে, আপনার `pom.xml` (Maven) বা `build.gradle` (Gradle) ফাইলে Logback এর জন্য নির্ভরতা যোগ করুন। Maven এর জন্য:

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

তারপর, ত্রুটি লগ করতে নিম্নলিখিত কোড ব্যবহার করতে পারেন:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerExample {
    private static final Logger logger = LoggerFactory.getLogger(LoggerExample.class);
    
    public static void main(String[] args) {
        try {
            int result = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("Error: Cannot divide by zero.", e);
        }
    }
}
```

এটি কনসোল বা একটি ফাইলে ত্রুটি বার্তা সহ স্ট্যাক ট্রেস আউটপুট করবে, Logback কনফিগারেশনের উপর নির্ভর করে।

Logback এর মতো লগিং ফ্রেমওয়ার্ক ব্যবহার করে ত্রুটি হ্যান্ডলিংয়ের উপর আরও নিয়ন্ত্রণ পাওয়া যায়, যা বৃহত অ্যাপলিকেশন এবং সিস্টেমগুলি পরিচালনা করা সহজ করে তোলে।
