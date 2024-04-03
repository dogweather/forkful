---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:11.966387-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE\u09A4\
  \u09C7, YAML \u09AB\u09BE\u0987\u09B2\u0997\u09C1\u09B2\u09BF \u09A8\u09BF\u09AF\
  \u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\
  \u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09B9\u09AC\
  \u09C7 \u0995\u09BE\u09B0\u09A3 \u099C\u09BE\u09AD\u09BE \u09B8\u09CD\u099F\u09CD\
  \u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u098F\u09A1\u09BF\u09B6\u09A8\
  \u09C7 YAML \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u09A8\u09BF\u09B0\u09CD\u09AE\
  \u09BF\u09A4-\u0987\u09A8\u2026"
lastmod: '2024-03-17T18:47:43.926274-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE\u09A4\u09C7, YAML \u09AB\u09BE\u0987\u09B2\u0997\
  \u09C1\u09B2\u09BF \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09A4\
  \u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\
  \u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7 \u0995\u09BE\u09B0\u09A3 \u099C\u09BE\
  \u09AD\u09BE \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\
  \u09A1 \u098F\u09A1\u09BF\u09B6\u09A8\u09C7 YAML \u098F\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4-\u0987\u09A8 \u09B8\u09BE\u09AA\
  \u09CB\u09B0\u09CD\u099F \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\
  \u09A4 \u09A8\u09C7\u0987\u0964 \u098F\u0995\u099F\u09BF \u099C\u09A8\u09AA\u09CD\
  \u09B0\u09BF\u09AF\u09BC \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \ \u09B9\u09B2 SnakeYAML, \u09AF\u09BE YAML \u09A1\u09BE\u099F\u09BE \u09AA\u09BE\
  \u09B0\u09CD\u09B8\u09BF\u0982 \u098F\u09AC\u0982 \u099C\u09C7\u09A8\u09BE\u09B0\
  \u09C7\u099F\u09BF\u0982 \u09B8\u09B9\u099C\u09C7 \u0995\u09B0\u09BE\u09B0 \u09B8\
  \u09C1\u09AF\u09CB\u0997 \u0995\u09B0\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964\n\n#."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

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
