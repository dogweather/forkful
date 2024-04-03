---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:14.858521-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u099C\u09BE\u09AD\u09BE\u09AF\u09BC `java.util.logging` \u09AA\u09CD\u09AF\u09BE\
  \u0995\u09C7\u099C \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u09B2\u0997\u09BF\u0982 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE\u09B0 \u098F\
  \u0995\u099F\u09BF \u09B8\u09B9\u099C \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\
  \u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u09CB\u0964."
lastmod: '2024-03-17T18:47:43.911233-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 \u099C\u09BE\u09AD\u09BE\u09AF\u09BC `java.util.logging`\
  \ \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u09B2\u0997\u09BF\u0982 \u09B6\u09C1\u09B0\u09C1 \u0995\
  \u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u0989\u09AA\u09BE\
  \u09AF\u09BC \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u09CB\u0964."
title: "\u09B2\u0997\u09BF\u0982"
weight: 17
---

## কিভাবে:
এখানে জাভায় `java.util.logging` প্যাকেজ ব্যবহার করে লগিং শুরু করার একটি সহজ উপায় দেওয়া হলো।

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Logging an INFO-level message");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Exception occur", e);
        }
    }
}
```

এটি প্রায় এইরকম আউটপুট উৎপাদন করবে:

```
জুলাই 03, 2023 2:00:00 PM AppLogging main
INFO: Logging an INFO-level message
জুলাই 03, 2023 2:00:00 PM AppLogging main
SEVERE: Exception occur
java.lang.ArithmeticException: / by zero
    at AppLogging.main(AppLogging.java:10)
```

## গভীর দৃষ্টিপাত
জাভায় লগিং অনেক বিকশিত হয়েছে। ঐতিহাসিকভাবে, লগিং ছিল অধিকতর অনানুষ্ঠানিক, সিস্টেম আউটপুটস এবং স্ব-নির্মিত মেকানিজমের সাথে। তবে, মানদণ্ডীকরণের প্রয়োজন `Log4j` এবং `SLF4J` এর মতো লগিং APIs-এ নিয়ে এসেছে। `java.util.logging` প্যাকেজটি নিজেই JDK 1.4-এ প্রবর্তিত হয়েছিল, মেসেজ লগ করার একটি মানদণ্ডীকৃত উপায় প্রদান করে।

`java.util.logging` (JUL) এর বিকল্প হিসেবে Log4j 2 এবং SLF4J রয়েছে। যদিও JUL জাভার ভিতরে বিল্ট-ইন এবং অতিরিক্ত নির্ভরতা ছাড়াই কাজ করে, Log4j 2 এবং SLF4J উন্নত বৈশিষ্ট্য অফার করে যেমন লগিং কনফিগারেশনের উপর আরও নির্ণায়ক নিয়ন্ত্রণ, অ্যাসিঙ্ক্রোনাস লগিং, এবং উত্তম পারফরম্যান্স।

কার্যকরী দিক থেকে, লগিং হতে পারে সমক্রমূক। এর মানে প্রতিটি লগ বার্তা তৈরি করা থ্রেডে প্রক্রিয়া জাত হয়, অথবা অসমক্রমূক, যেখানে বার্তাগুলি আলাদা থ্রেডে হস্তান্তরিত হয়। অসমক্রমূক লগিং পারফরম্যান্স উন্নতি করতে পারে কিন্তু জটিলতা প্রবর্তন করে যেখানে একজনকে সমান্তরালিতা সামলাতে এবং নিশ্চিত করতে হবে যে অ্যাপ্লিকেশন ক্র্যাশের সময় লগ বার্তা হারিয়ে যায় না।

## আরও দেখুন
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Oracle-এর অফিসিয়াল লগিং ওভারভিউ](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [java.util.logging সম্পর্কে টিউটোরিয়াল](https://www.vogella.com/tutorials/Logging/article.html)
