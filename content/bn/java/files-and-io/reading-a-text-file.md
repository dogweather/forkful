---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:02.159880-06:00
description: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\
  \u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u098F\u0995\
  \u099F\u09BF \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 \u0995\u09A8\u09CD\
  \u099F\u09C7\u09A8\u09CD\u099F \u0986\u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\
  \u09B0\u09C7 \u09A5\u09BE\u0995\u09C7 \u09A1\u09BF\u09B8\u09CD\u0995\u09C7 \u09A5\
  \u09BE\u0995\u09BE \u09AB\u09BE\u0987\u09B2\u09C7 \u09AC\u09B8\u09C7 \u09A1\u09C7\
  \u099F\u09BE \u09AA\u09CD\u09B0\u09B8\u09C7\u09B8 \u0995\u09B0\u09BE \u09AC\u09BE\
  \u2026"
lastmod: '2024-03-17T18:47:43.922275-06:00'
model: gpt-4-0125-preview
summary: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB \u0986\u09AA\u09A8\u09BE\
  \u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u098F\u0995\u099F\
  \u09BF \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 \u0995\u09A8\u09CD\u099F\
  \u09C7\u09A8\u09CD\u099F \u0986\u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\
  \u09C7 \u09A5\u09BE\u0995\u09C7 \u09A1\u09BF\u09B8\u09CD\u0995\u09C7 \u09A5\u09BE\
  \u0995\u09BE \u09AB\u09BE\u0987\u09B2\u09C7 \u09AC\u09B8\u09C7 \u09A1\u09C7\u099F\
  \u09BE \u09AA\u09CD\u09B0\u09B8\u09C7\u09B8 \u0995\u09B0\u09BE \u09AC\u09BE \u09AC\
  \u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF\u0964 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8\
  , \u09A1\u09C7\u099F\u09BE \u0985\u09CD\u09AF\u09BE\u09A8\u09BE\u09B2\u09BE\u0987\
  \u09B8\u09BF\u09B8, \u0985\u09A5\u09AC\u09BE \u098F\u09AE\u09A8\u0995\u09BF \u09B6\
  \u09C1\u09A7\u09C1 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\u09B0\u09CD\u09AF\
  \u09A4\u09BE\u09B2\u09BF\u0995\u09BE \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u09B0\
  \ \u09AE\u09A4 \u0995\u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\
  \u09BF \u0985\u09A4\u09CD\u09AF\u09A8\u09CD\u09A4 \u099C\u09B0\u09C1\u09B0\u09C0\
  \u0964."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কীভাবে:
জাভাতে একটি ফাইল পড়া খুবই সহজ, বিশেষ করে `java.nio.file` এর সাথে। এখানে একটি দ্রুত উদাহরণ দেওয়া হলো:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.io.IOException;
import java.util.stream.Stream;

public class FileReadExample {
    public static void main(String[] args) {
        Path filePath = Path.of("example.txt");

        try (Stream<String> lines = Files.lines(filePath)) {
            lines.forEach(System.out::println);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

এটি `example.txt` ফাইলটি যদি "Hello, file readers!" ধারণ করে থাকে তাহলে আউটপুট হবে:

```
Hello, file readers!
```

## গভীরে যাওয়া:
জাভা বিকশিত হয়েছে। আগের দিনে, আপনাকে নিজে স্ট্রিম এবং রিডারগুলি ম্যানেজ করতে হতো - প্রচুর বয়লারপ্লেট। `java.io` প্যাকেজটি সব সময় জনপ্রিয় ছিল, `FileReader` এবং `BufferedReader` প্রায়ই দেখা যেত। তারপর `java.nio` আসে, যা আরো নিয়ন্ত্রণের জন্য চ্যানেল এবং বাফার অফার করে।

এখন, `java.nio.file` আরও উচ্চতর স্তরের। `Files` এবং `Paths` কাজ সহজ করে দেয়। উপরের উদাহরণে `Files.lines` ব্যবহৃত হয়েছে, যেটি বড় ফাইলগুলিতে জন্য লাইনগুলি অলসভাবে স্ট্রীম করে, অসাধারণ। আপনি স্ট্রিমগুলি বন্ধ করে ফাঁসি এড়াতে ট্রাই-উইথ-রিসোর্সেসও পান।

বিকল্প? `Scanner` পার্সিং এর জন্য সুবিধাজনক। আপাচি কমন্স IO এবং গুগলের গুয়াভা আরও জটিল কাজের জন্য ইউটিলিটিজ অফার করে, যদি আপনার প্রয়োজন হয়। তবে, সাধারণত জাভা প্রায়ই আপনাকে বেশ দূরে নিয়ে যায়।

বাস্তবায়নের দিক থেকে, ফাইল এনকোডিং গুরুত্বপূর্ণ। `Files.lines` ডিফল্ট হিসেবে UTF-8 ধরে নেয় কিন্তু আপনি অন্য কিছু নির্দিষ্ট করতে পারেন। অন্যদিকে, `BufferedReader` যদি ডিফল্ট না হয় তাহলে আপনাকে `Charset` আগে থেকে সেট করতে হবে।

## দেখুন এছাড়াও
আরও জানতে  এগুলো দেখুন:

- জাভার অফিসিয়াল ডকুমেন্টেশনে জাভার [`Files`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html) ক্লাস।
- [ফাইল পড়া, লেখা, এবং তৈরি করা](https://docs.oracle.com/javase/tutorial/essential/io/file.html) একটি বিস্তারিত পথচলার জন্য।
- [Apache Commons IO](https://commons.apache.org/proper/commons-io/) ফাইল IO ইউটিলিটিজের একটি দৃঢ় লাইব্রেরির জন্য।
