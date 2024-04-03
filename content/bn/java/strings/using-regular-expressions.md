---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:30.682189-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\
  \u09CD\u09B8\u09AA\u09CD\u09B0\u09C7\u09B6\u09A8\u09C7\u09B0 \u09AC\u09BF\u09B2\u09CD\
  \u099F-\u0987\u09A8 \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u09AE\u09C2\u09B2\
  \u09A4 `java.util.regex` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u09C7\u09B0\
  \ `Pattern` \u098F\u09AC\u0982 `Matcher` \u0995\u09CD\u09B2\u09BE\u09B8\u09C7\u09B0\
  \ \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\u0964 \u098F\u0995\u099F\u09BF \u09B8\
  \u09B0\u09B2 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u098F\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.893633-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09B0\u09C7\u0997\
  \u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\u09B0\u09C7\u09B6\u09A8\
  \u09C7\u09B0 \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09B8\u09BE\u09AA\u09CB\
  \u09B0\u09CD\u099F \u09AE\u09C2\u09B2\u09A4 `java.util.regex` \u09AA\u09CD\u09AF\
  \u09BE\u0995\u09C7\u099C\u09C7\u09B0 `Pattern` \u098F\u09AC\u0982 `Matcher` \u0995\
  \u09CD\u09B2\u09BE\u09B8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\u0964\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09B0\u09B2 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\
  \ \u098F\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u098F\u0995\
  \u099F\u09BF \u09B6\u09AC\u09CD\u09A6\u09C7\u09B0 \u09B8\u09AC \u0989\u09A6\u09CD\
  \u09AD\u09AC \u0996\u09C1\u0981\u099C\u09C7 \u09AC\u09C7\u09B0 \u0995\u09B0\u09C7\
  \ \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE, \u0995\u09C7\u0987\
  \u09B8 \u0985\u09B8\u0982\u09AC\u09C7\u09A6\u09C0."
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

## কিভাবে:
জাভার জন্য রেগুলার এক্সপ্রেশনের বিল্ট-ইন সাপোর্ট মূলত `java.util.regex` প্যাকেজের `Pattern` এবং `Matcher` ক্লাসের মাধ্যমে। একটি সরল উদাহরণ এর মাধ্যমে একটি স্ট্রিং থেকে একটি শব্দের সব উদ্ভব খুঁজে বের করে প্রিন্ট করা, কেইস অসংবেদী:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex পার্স করার জন্য দারুণ। পার্স করা সাথে regex শক্তিশালী।";
        String wordToFind = "পার্স";
        
        Pattern pattern = Pattern.compile(wordToFind, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("খুঁজে পেয়েছি '" + matcher.group() + "' পজিশনে " + matcher.start());
        }
    }
}
```

আউটপুট:
```
খুঁজে পেয়েছি 'পার্স' পজিশনে 16
খুঁজে পেয়েছি 'পার্স' পজিশনে 31
```

স্ট্রিং স্প্লিট করার মতো কাজ জন্য, আপনি রেগুলার এক্সপ্রেশন সহ `String` ক্লাসের `split()` মেথড ব্যবহার করতে পারেন:

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] languages = text.split(",");
        
        for (String language : languages) {
            System.out.println(language);
        }
    }
}
```

আউটপুট:
```
Java
Python
Ruby
JavaScript
```

জাভার সাথে রেগুলার এক্সপ্রেশন কাজ করার সময়, জটিল কাজগুলি সহজ করার জন্য বাইরের লাইব্রেরির প্রয়োজন হতে পারে। জাভার সাথে রেগুলার এক্সপ্রেশন কাজ করার জন্য একটি জনপ্রিয় থার্ড-পার্টি লাইব্রেরি হল `Apache Commons Lang`। এটি `StringUtils` এর মতো ইউটিলিটি অফার করে যা কিছু রেগুলার এক্সপ্রেশন টাস্ককে আরও সোজা করে দেয়। এটিকে কিভাবে ব্যবহার করবেন এখানে বলা হয়েছে একটি উপস্থিতি গণনা করার জন্য:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex টেক্সট প্রসেসিংকে সহজ করে। টেক্সট প্রসেসিং সাথে regex কার্যকর।";
        String substring = "প্রসেসিং";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "' " + count + " বার উপস্থিত আছে।");
    }
}
```

Apache Commons Lang ব্যবহার করতে, আপনার প্রকল্পে এটি অন্তর্ভুক্ত করতে হবে। যদি আপনি Maven ব্যবহার করছেন, তাহলে আপনার `pom.xml`-এ এই ডিপেন্ডেন্সি যোগ করুন:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- সর্বশেষ ভার্শনের জন্য চেক করুন -->
</dependency>
```

আউটপুট:
```
'প্রসেসিং' 2 বার উপস্থিত আছে।
```
