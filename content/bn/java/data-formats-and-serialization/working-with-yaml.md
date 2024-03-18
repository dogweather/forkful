---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:11.966387-06:00
description: "YAML, \"YAML Ain't Markup Language\" \u098F\u09B0 \u09B8\u0982\u0995\
  \u09CD\u09B7\u09BF\u09AA\u09CD\u09A4 \u09B0\u09C2\u09AA, \u098F\u0995\u099F\u09BF\
  \ \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF \u09A1\
  \u09BE\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\
  \u09C7\u09B6\u09A8 \u09AE\u09BE\u09A8\u09A6\u09A3\u09CD\u09A1 \u09AF\u09BE \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0995\u09A8\
  \u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2, \u09A1\
  \u09BE\u099F\u09BE \u09A1\u09BE\u09AE\u09CD\u09AA\u09BF\u0982, \u098F\u09AC\u0982\
  \u2026"
lastmod: '2024-03-17T18:47:43.926274-06:00'
model: gpt-4-0125-preview
summary: "YAML, \"YAML Ain't Markup Language\" \u098F\u09B0 \u09B8\u0982\u0995\u09CD\
  \u09B7\u09BF\u09AA\u09CD\u09A4 \u09B0\u09C2\u09AA, \u098F\u0995\u099F\u09BF \u09AE\
  \u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF \u09A1\u09BE\
  \u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\
  \u09B6\u09A8 \u09AE\u09BE\u09A8\u09A6\u09A3\u09CD\u09A1 \u09AF\u09BE \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0995\u09A8\u09AB\
  \u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2, \u09A1\u09BE\
  \u099F\u09BE \u09A1\u09BE\u09AE\u09CD\u09AA\u09BF\u0982, \u098F\u09AC\u0982\u2026"
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
YAML, "YAML Ain't Markup Language" এর সংক্ষিপ্ত রূপ, একটি মানব-পাঠযোগ্য ডাটা সিরিয়ালাইজেশন মানদণ্ড যা প্রোগ্রামাররা কনফিগারেশন ফাইল, ডাটা ডাম্পিং, এবং ভাষার মধ্যে ডাটা সংক্রমণের জন্য ব্যবহার করে। এটি এর পঠনযোগ্যতা এবং ব্যবহারের সহজতার কারণে জনপ্রিয়, যা এটিকে অ্যাপ্লিকেশন এবং সার্ভিস কনফিগারেশনের জন্য একটি সাধারণ পছন্দে পরিণত করেছে।

## কিভাবে:
জাভাতে, YAML ফাইলগুলি নিয়ে কাজ করার জন্য আপনাকে তৃতীয়-পক্ষের লাইব্রেরি ব্যবহার করতে হবে কারণ জাভা স্ট্যান্ডার্ড এডিশনে YAML এর জন্য নির্মিত-ইন সাপোর্ট অন্তর্ভুক্ত নেই। একটি জনপ্রিয় লাইব্রেরি হল SnakeYAML, যা YAML ডাটা পার্সিং এবং জেনারেটিং সহজে করার সুযোগ করে দেয়।

### SnakeYAML সেট আপ করা
প্রথমে, SnakeYAML আপনার প্রজেক্টে অন্তর্ভুক্ত করুন। আপনি যদি Maven ব্যবহার করেন, তাহলে নিম্নলিখিত নির্ভরতা `pom.xml`-এ যোগ করুন:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### YAML পড়া
```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class ReadYamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        try (InputStream inputStream = ReadYamlExample.class
                .getClassLoader()
                .getResourceAsStream("config.yml")) {
            Map<String, Object> data = yaml.load(inputStream);
            System.out.println(data);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
ধরুন `config.yml` এরুপ দেখতে:
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
আউটপুট হবে:
```
{name=Example, version=1.0, features=[login, signup]}
```

### YAML লেখা
জাভা অবজেক্ট থেকে YAML জেনারেট করতে, SnakeYAML এর দ্বারা প্রদান করা `dump` পদ্ধতিটি ব্যবহার করুন:
```java
import org.yaml.snakeyaml.Yaml;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

public class WriteYamlExample {
    public static void main(String[] args) {
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("name", "Example");
        data.put("version", 1.0);
        data.put("features", Arrays.asList("login", "signup"));

        Yaml yaml = new Yaml();
        String output = yaml.dump(data);
        System.out.println(output);
    }
}
```
এটি নিম্নলিখিত YAML কন্টেন্ট জেনারেট এবং প্রিন্ট করবে:
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
SnakeYAML কে ব্যবহার করে, জাভা ডেভেলপাররা সহজেই YAML পার্সিং এবং জেনারেশন তাদের অ্যাপ্লিকেশনে একীভূত করতে পারে, YAML এর পঠনযোগ্যতা এবং সাদাসিধেতা থেকে কনফিগারেশন এবং ডাটা বিনিময় উদ্দেশ্যে সুফল পেতে পারে।
