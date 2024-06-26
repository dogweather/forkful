---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:43:08.623555-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Kotlin \u098F, stderr \u098F \u09B2\
  \u09BF\u0996\u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7 `System.err.println()`\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964 \u098F\u0987\
  \ \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u099F\u09BF `System.out.println()` \u098F\
  \u09B0 \u09AE\u09A4\u09CB, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u0986\u0989\u099F\
  \u09AA\u09C1\u099F\u099F\u09BF\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\u2026"
lastmod: '2024-03-17T18:47:44.010904-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u098F, stderr \u098F \u09B2\u09BF\u0996\u09BE \u09AF\u09C7\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7 `System.err.println()` \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7\u0964 \u098F\u0987 \u09AA\u09A6\u09CD\u09A7\u09A4\
  \u09BF\u099F\u09BF `System.out.println()` \u098F\u09B0 \u09AE\u09A4\u09CB, \u0995\
  \u09BF\u09A8\u09CD\u09A4\u09C1 \u0986\u0989\u099F\u09AA\u09C1\u099F\u099F\u09BF\u0995\
  \u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u09AE\u09C7\u09B0 \u09A6\
  \u09BF\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6 \u0995\u09B0\u09C7\
  \ \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u09AE\
  \u09C7\u09B0 \u09AC\u09A6\u09B2\u09C7\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কিভাবে:
Kotlin এ, stderr এ লিখা যেতে পারে `System.err.println()` ব্যবহার করে। এই পদ্ধতিটি `System.out.println()` এর মতো, কিন্তু আউটপুটটিকে স্ট্যান্ডার্ড এরর স্ট্রিমের দিকে নির্দেশ করে স্ট্যান্ডার্ড আউটপুট স্ট্রিমের বদলে।

```kotlin
fun main() {
    System.err.println("এটি একটি ত্রুটি বার্তা!")
}
```

নমুনা আউটপুট:
```
এটি একটি ত্রুটি বার্তা!
```

আরও গঠনমূলক বা জটিল অ্যাপ্লিকেশনগুলির জন্য, বিশেষ করে যেগুলি লগিং ফ্রেমওয়ার্ক যেমন Logback বা SLF4J ব্যবহার করে, নির্দিষ্ট লগ লেভেলগুলির জন্য (যেমন, ERROR) stderr এ লিখতে লগারগুলি কনফিগার করা যেতে পারে।

SLF4J দিয়ে Logback ব্যবহার করা:

1. প্রথমে, আপনার `build.gradle` এ SLF4J API এবং Logback বাস্তবায়ন যোগ করুন:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. পরবর্তীতে, Logback কনফিগার করুন (ইন `src/main/resources/logback.xml`) যাতে ত্রুটি-স্তরের বার্তাগুলি stderr এ নির্দেশ করা হয়:

```xml
<configuration>
    <appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
        <target>System.err</target>
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>
    
    <root level="error">
        <appender-ref ref="STDERR" />
    </root>
</configuration>
```

3. তারপরে, আপনার Kotlin কোডে SLF4J ব্যবহার করে ত্রুটি বার্তাগুলি লগ করুন:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("এটি একটি ত্রুটি লগ বার্তা!")
}
```

নমুনা আউটপুট (stderr এ):
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - এটি একটি ত্রুটি লগ বার্তা!
```
