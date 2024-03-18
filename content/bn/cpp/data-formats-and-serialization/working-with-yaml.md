---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:40.468338-06:00
description: "YAML, \u09AF\u09BE\u09B0 \u09B8\u09AE\u09CD\u09AA\u09C2\u09B0\u09CD\u09A3\
  \ \u09A8\u09BE\u09AE YAML Ain't Markup Language, \u098F\u0995\u099F\u09BF \u09AE\
  \u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09CD\u09AF \u09A1\u09BE\u099F\u09BE \u09B8\
  \u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AB\
  \u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3 XML \u09AC\u09BE JSON \u098F\u09B0 \u09A4\
  \u09C1\u09B2\u09A8\u09BE\u09AF\u09BC \u098F\u09B0 \u09AA\u09BE\u09A0\u09AF\u09CB\
  \u0997\u09CD\u09AF\u09A4\u09BE \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.385777-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u09AF\u09BE\u09B0 \u09B8\u09AE\u09CD\u09AA\u09C2\u09B0\u09CD\u09A3\
  \ \u09A8\u09BE\u09AE YAML Ain't Markup Language, \u098F\u0995\u099F\u09BF \u09AE\
  \u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09CD\u09AF \u09A1\u09BE\u099F\u09BE \u09B8\
  \u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AB\
  \u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3 XML \u09AC\u09BE JSON \u098F\u09B0 \u09A4\
  \u09C1\u09B2\u09A8\u09BE\u09AF\u09BC \u098F\u09B0 \u09AA\u09BE\u09A0\u09AF\u09CB\
  \u0997\u09CD\u09AF\u09A4\u09BE \u098F\u09AC\u0982\u2026"
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

YAML, যার সম্পূর্ণ নাম YAML Ain't Markup Language, একটি মানব-পাঠ্য ডাটা সিরিয়ালাইজেশন ফর্ম্যাট। প্রোগ্রামারগণ XML বা JSON এর তুলনায় এর পাঠযোগ্যতা এবং সহজ-বোধ্য সিনট্যাক্সের কারণে কনফিগুরেশন ফাইল, ডাটা ডাম্পিং এবং স্তরবদ্ধ ডাটা সংরক্ষণের জন্য এটি ব্যবহার করে থাকেন।

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
