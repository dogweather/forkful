---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:40.468338-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C++ \u098F YAML \u098F\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09A4\u09C7, \u099C\u09A8\u09AA\
  \u09CD\u09B0\u09BF\u09AF\u09BC \u098F\u0995\u099F\u09BF \u09AA\u099B\u09A8\u09CD\
  \u09A6 \u09B9\u09B2 `yaml-cpp` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\
  \u09A4 \u0995\u09B0\u09C1\u09A8 \u0986\u09AA\u09A8\u09BF `yaml-cpp` \u0987\u09A8\
  \u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09C7\u099B\u09C7\u09A8 \u098F\u09AC\u0982\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 C++\u2026"
lastmod: '2024-03-17T18:47:44.385777-06:00'
model: gpt-4-0125-preview
summary: "C++ \u098F YAML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09A4\u09C7, \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u098F\
  \u0995\u099F\u09BF \u09AA\u099B\u09A8\u09CD\u09A6 \u09B9\u09B2 `yaml-cpp` \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\
  \u09C7, \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09C1\u09A8 \u0986\
  \u09AA\u09A8\u09BF `yaml-cpp` \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09C7\
  \u099B\u09C7\u09A8 \u098F\u09AC\u0982 \u0986\u09AA\u09A8\u09BE\u09B0 C++ \u09AA\u09CD\
  \u09B0\u099C\u09C7\u0995\u09CD\u099F\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B8\
  \u09A0\u09BF\u0995\u09AD\u09BE\u09AC\u09C7 \u09B2\u09BF\u0982\u0995 \u0995\u09B0\
  \u09C7\u099B\u09C7\u09A8\u0964\n\n**YAML \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\
  \u09BE:**."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

## কিভাবে:
C++ এ YAML এর সাথে কাজ করতে, জনপ্রিয় একটি পছন্দ হল `yaml-cpp` লাইব্রেরি। প্রথমে, নিশ্চিত করুন আপনি `yaml-cpp` ইনস্টল করেছেন এবং আপনার C++ প্রজেক্টের সাথে সঠিকভাবে লিংক করেছেন।

**YAML ফাইল পড়া:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "Title: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

ধরুন একটি `config.yaml` ফাইল এরকম দেখতে:

```yaml
title: "Example YAML"
```

উপরের C++ কোড চালানো হলে ফলাফল হবে:

```
Title: Example YAML
```

**YAML ফাইলে লিখা:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "title" << YAML::Value << "Example YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

এই কোডটি `output.yaml` নামক একটি ফাইল তৈরি করবে যাতে থাকবে:

```yaml
title: Example YAML
```

এই উদাহরণগুলি C++ ব্যবহার করে YAML ফাইল থেকে পড়া এবং তাতে লিখার মৌলিক পরিচয় হিসেবে কাজ করে যা `yaml-cpp` লাইব্রেরি ব্যবহার করে সম্পন্ন হয়। আরও জটিল কাঠামো এবং ব্যবহারের ক্ষেত্রগুলি আন্বেষণ করতে, `yaml-cpp` ডকুমেন্টেশন দেখুন যেখানে অনুক্রম, ট্যাগ এবং আরও উন্নত সিরিয়ালাইজেশন এবং ডিসিরিয়ালাইজেশন কৌশল সম্পর্কে বিস্তৃত তথ্য আছে।
