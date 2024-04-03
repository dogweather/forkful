---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:43.231680-06:00
description: "\u09AE\u09BE\u09A8\u0995 \u09A4\u09CD\u09B0\u09C1\u099F\u09BF (stderr)\
  \ \u098F \u09B2\u09C7\u0996\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09A4\u09CD\
  \u09B0\u09C1\u099F\u09BF \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE \u098F\u09AC\u0982\
  \ \u09A1\u09BE\u09DF\u09BE\u0997\u09A8\u09B8\u09CD\u099F\u09BF\u0995\u09B8 \u0995\
  \u09A8\u09B8\u09CB\u09B2 \u09AC\u09BE \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\
  \u09BE\u09B2\u09C7 \u0986\u0989\u099F\u09AA\u09C1\u099F \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BE \u0995\u09B0\u09C7 \u09AE\u09BE\u09A8\u0995 \u0986\u0989\u099F\
  \u09AA\u09C1\u099F (stdout) \u09A5\u09C7\u0995\u09C7 \u09A4\u09CD\u09B0\u09C1\u099F\
  \u09BF\u2026"
lastmod: '2024-03-17T18:47:43.921315-06:00'
model: gpt-4-0125-preview
summary: "\u09AE\u09BE\u09A8\u0995 \u09A4\u09CD\u09B0\u09C1\u099F\u09BF (stderr) \u098F\
  \ \u09B2\u09C7\u0996\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09A4\u09CD\u09B0\
  \u09C1\u099F\u09BF \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE \u098F\u09AC\u0982 \u09A1\
  \u09BE\u09DF\u09BE\u0997\u09A8\u09B8\u09CD\u099F\u09BF\u0995\u09B8 \u0995\u09A8\u09B8\
  \u09CB\u09B2 \u09AC\u09BE \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\u09B2\
  \u09C7 \u0986\u0989\u099F\u09AA\u09C1\u099F \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BE\
  \ \u0995\u09B0\u09C7 \u09AE\u09BE\u09A8\u0995 \u0986\u0989\u099F\u09AA\u09C1\u099F\
  \ (stdout) \u09A5\u09C7\u0995\u09C7 \u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09A4\u09A5\
  \u09CD\u09AF \u09AA\u09C3\u09A5\u0995 \u0995\u09B0\u09A4\u09C7, \u09AF\u09BE \u09A1\
  \u09BF\u09AC\u09BE\u0997\u09BF\u0982 \u098F\u09AC\u0982 \u09B2\u0997 \u09AC\u09BF\
  \u09B6\u09CD\u09B2\u09C7\u09B7\u09A3\u09C7 \u09B8\u09B9\u09BE\u09DF\u0995\u0964."
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
