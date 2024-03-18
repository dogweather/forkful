---
title:                "স্ট্যান্ডার্ড এররে লিখন"
date:                  2024-03-17T18:43:08.623555-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

স্ট্যান্ডার্ড এরর (stderr) এ লিখা মানে হল ত্রুটি বার্তা এবং ডায়াগনস্টিকস আলাদা স্ট্রিমে, স্ট্যান্ডার্ড আউটপুট (stdout) থেকে পৃথক, আউটপুট করা যা ত্রুটি হ্যান্ডলিং এবং লগ পার্সিং উন্নত করে। প্রোগ্রামাররা এটি ডিবাগিং সহজতর করতে এবং নিশ্চিত করতে যে ত্রুটি বার্তাগুলি সহজে শনাক্ত করা এবং প্রয়োজনে পুনঃনির্দেশ করা যেতে পারে তা নিশ্চিত করার জন্য করে, যেন আউটপুট লগগুলি বা ব্যবহারকারীর বার্তাগুলি পরিষ্কার থাকে।

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
