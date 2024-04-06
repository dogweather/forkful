---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:40:01.567648-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B0\
  \ \u09A8\u09BF\u0989 \u0986\u0987/\u0993 (NIO) \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\
  \u099C (`java.nio.file`) \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 \u09A1\u09BF\u09B2\u09BF\u0982 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u0986\u09B0\u0993 \u09AC\u09B9\u09C1\u09AE\u09C1\u0996\u09C0 \u09AA\u09A6\
  \u09CD\u09A7\u09A4\u09BF \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\
  \u0964 `Files.write()` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u09AB\u09BE\u0987\u09B2\u09C7 \u09B0\u09BE\u0987\u099F\u2026"
lastmod: '2024-03-17T18:47:43.923666-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B0 \u09A8\u09BF\u0989 \u0986\u0987/\u0993 (NIO)\
  \ \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C (`java.nio.file`) \u09AB\u09BE\u0987\
  \u09B2\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09A1\u09BF\u09B2\u09BF\u0982 \u0995\
  \u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0986\u09B0\u0993 \u09AC\u09B9\u09C1\
  \u09AE\u09C1\u0996\u09C0 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09AA\u09CD\u09B0\
  \u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 `Files.write()` \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09AB\u09BE\u0987\u09B2\u09C7 \u09B0\u09BE\
  \u0987\u099F \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09B8\u09B0\u09B2\
  \ \u0989\u09AA\u09BE\u09AF\u09BC \u09A8\u09BF\u099A\u09C7 \u09A6\u09C7\u0993\u09AF\
  \u09BC\u09BE \u09B9\u09B2\u0983."
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কিভাবে:


### `java.nio.file` ব্যবহার করে (মানক লাইব্রেরি)
জাভার নিউ আই/ও (NIO) প্যাকেজ (`java.nio.file`) ফাইলের সাথে ডিলিং করার জন্য আরও বহুমুখী পদ্ধতি প্রদান করে। `Files.write()` ব্যবহার করে ফাইলে রাইট করার একটি সরল উপায় নিচে দেওয়া হলঃ

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> lines = Arrays.asList("লাইন ১", "লাইন ২", "লাইন ৩");
        try {
            Files.write(Paths.get("example.txt"), lines);
            System.out.println("ফাইল সফলভাবে লিখা হয়েছে!");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

আউটপুট:

```
ফাইল সফলভাবে লিখা হয়েছে!
```

### `java.io` ব্যবহার করে (মানক লাইব্রেরি)
একটি আরও পরম্পরাগত দৃষ্টিভঙ্গির জন্য, `java.io.FileWriter` টেক্সট ফাইল সহজে লিখার জন্য একটি ভাল বিকল্প:

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("example.txt")) {
            writer.write("হ্যালো, ওয়ার্ল্ড!\n");
            writer.append("এটি আরেকটি লাইন।");
            System.out.println("ফাইল সফলভাবে লিখা হয়েছে!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

আউটপুট:

```
ফাইল সফলভাবে লিখা হয়েছে!
```

### অ্যাপাচি কমন্স আইও ব্যবহার করে
অ্যাপাচি কমন্স আইও লাইব্রেরি অনেক অপারেশন, ফাইল রাইটিং সহ সহজ করে দেয়। `FileUtils.writeStringToFile()` ব্যবহার করে ফাইলে লেখার পদ্ধতি নিচে দেওয়া হলঃ:

প্রথমে, আপনার প্রজেক্টে নির্ভরতা যোগ করুন। যদি মেভেন ব্যবহার করেন, যুক্ত করুনঃ

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- সর্বশেষ ভার্সনের জন্য চেক করুন -->
</dependency>
```

তারপর, নিচের কোড ব্যবহার করে ফাইলে টেক্সট লেখাঃ

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("example.txt"), "এটি কমন্স আইও ব্যবহার করে লেখা টেক্সট।", "UTF-8");
            System.out.println("ফাইল সফলভাবে লিখা হয়েছে!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

আউটপুট:

```
ফাইল সফলভাবে লিখা হয়েছে!
```
