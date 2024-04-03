---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:33.695688-06:00
description: "TOML \u09AE\u09BE\u09A8\u09C7 Tom's Obvious, Minimal Language\u0964\
  \ \u098F\u099F\u09BF \u098F\u0995\u099F\u09BF \u09A1\u09BE\u099F\u09BE \u09B8\u09BF\
  \u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AB\u09B0\
  \u09CD\u09AE\u09CD\u09AF\u09BE\u099F \u09AF\u09BE \u0995\u09A8\u09AB\u09BF\u0997\
  \ \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09C3\u09A4 \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0995\u09BE\u09B0\u09A3 \u098F\
  \u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:43.929228-06:00'
model: gpt-4-0125-preview
summary: "TOML \u09AE\u09BE\u09A8\u09C7 Tom's Obvious, Minimal Language\u0964 \u098F\
  \u099F\u09BF \u098F\u0995\u099F\u09BF \u09A1\u09BE\u099F\u09BE \u09B8\u09BF\u09B0\
  \u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AB\u09B0\u09CD\
  \u09AE\u09CD\u09AF\u09BE\u099F \u09AF\u09BE \u0995\u09A8\u09AB\u09BF\u0997 \u09AB\
  \u09BE\u0987\u09B2\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09C3\u09A4 \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0995\u09BE\u09B0\u09A3 \u098F\u099F\
  \u09BF \u09AA\u09A1\u09BC\u09BE, \u09B2\u09C7\u0996\u09BE \u09B8\u09B9\u099C \u098F\
  \u09AC\u0982 \u098F\u0995\u099F\u09BF \u09B9\u09CD\u09AF\u09BE\u09B6 \u099F\u09C7\
  \u09AC\u09BF\u09B2\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B8\u09C1\u09A8\u09CD\
  \u09A6\u09B0\u09AD\u09BE\u09AC\u09C7 \u09AE\u09CD\u09AF\u09BE\u09AA \u0995\u09B0\
  \u09C7\u0964."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কিভাবে:
আপনার একটি TOML পার্সিং লাইব্রেরির প্রয়োজন হবে। আমি `toml4j` সুপারিশ করি। আপনার প্রজেক্টে এটি যোগ করুন এই ভাবে:

```java
// এটি আপনার build.gradle এ যোগ করুন
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

এইভাবে আপনি একটি TOML ফাইল পার্স করবেন:

```java
import com.moandjiezana.toml.Toml;

public class TomlExample {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("সার্ভার আইপি: " + ip);
        System.out.println("সার্ভার পোর্ট: " + port);
    }
}
```

নমুনা আউটপুট:

```
সার্ভার আইপি: 192.168.1.1
সার্ভার পোর্ট: 80
```

## গভীরে ডুব:
GitHub সহ-প্রতিষ্ঠাতা টম প্রেস্টন-ওয়ার্নার দ্বারা উন্নত, TOML XML এর চেয়ে সহজ এবং YAML এর চেয়ে স্পেসিফিক হতে লক্ষ্য করেছিল। এর সর্বশেষ সংস্করণ 1.0.0, 2021 এ মুক্তি পেয়েছিল, যা একটি স্থিতিশীল বৈশিষ্ট্য সেট অফার করে।

JSON বা YAML এর মতো বিকল্পও জনপ্রিয়। JSON ডাটা ইন্টারচেঞ্জের জন্য দারুণ। YAML জটিল কনফিগের জন্য আরও মানবপঠনযোগ্য। TOML এর শক্তি হল এর সরলতা এবং রাস্ট কমিউনিটিতে এর ব্যবহার।

TOML কে জাভা সাথে ব্যবহার করার সময়, মনে রাখবেন যে আপনি যে পার্সারটি বেছে নেন তা গুরুত্বপূর্ণ। `toml4j` ছাড়াও, কেউ কেউ `jackson-dataformat-toml` এর দিকে যান। প্রত্যেকের নিজস্ব নুয়ান্স থাকবে, যেমন এরর হ্যান্ডলিং বা পার্সিং পারফরমেন্স, তাই আপনার প্রজেক্টের প্রয়োজনের ভিত্তিতে নির্বাচন করুন।

## দেখুন
- TOML স্পেসিফিকেশন: https://toml.io/en/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
