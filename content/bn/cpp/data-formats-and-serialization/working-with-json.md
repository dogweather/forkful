---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:04.861927-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C++ \u098F JSON \u098F\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u0995\u09CB\u09A8\u09CB \u09A8\u09C7\u099F\u09BF\u09AD \u09B8\
  \u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u09A8\u09C7\u0987, \u09A4\u09AC\u09C7 nlohmann/json\
  \ \u098F\u09B0 \u09AE\u09A4\u09CB \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\
  \u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \ \u098F\u099F\u09BF\u0995\u09C7 \u09B8\u09B0\u09B2 \u0995\u09B0\u09C7 \u09A4\u09CB\
  \u09B2\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09AC\u09C7\u09B8\u09BF\u0995\
  \ \u0995\u09BE\u099C\u0997\u09C1\u09B2\u09CB\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\
  \u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.386699-06:00'
model: gpt-4-0125-preview
summary: "C++ \u098F JSON \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09CB\u09A8\
  \u09CB \u09A8\u09C7\u099F\u09BF\u09AD \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F\
  \ \u09A8\u09C7\u0987, \u09A4\u09AC\u09C7 nlohmann/json \u098F\u09B0 \u09AE\u09A4\
  \u09CB \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u098F\u099F\u09BF\u0995\
  \u09C7 \u09B8\u09B0\u09B2 \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7 \u09AC\u09C7\u09B8\u09BF\u0995 \u0995\u09BE\u099C\u0997\
  \u09C1\u09B2\u09CB\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF \u0995\u09BF\
  \u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE \u09AF\u09BE\u09AF\u09BC."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কিভাবে:
C++ এ JSON এর জন্য কোনো নেটিভ সাপোর্ট নেই, তবে nlohmann/json এর মতো তৃতীয়-পক্ষের লাইব্রেরি এটিকে সরল করে তোলে। এখানে বেসিক কাজগুলোর জন্য এটি কিভাবে ব্যবহার করা যায়:

প্রথমে, আপনার লাইব্রেরিটি ইনস্টল করা আছে কিনা তা নিশ্চিত করুন। আপনি যদি vcpkg বা Conan এর মতো প্যাকেজ ম্যানেজার ব্যবহার করেন, তাহলে নিজের প্রোজেক্টে `nlohmann/json` অ্যাড করতে সহজেই পারবেন।

### স্ট্রিং থেকে JSON পার্সিং
```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // স্ট্রিং হিসাবে JSON ডাটা
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // JSON স্ট্রিং পার্স করা
    auto jsonObject = nlohmann::json::parse(jsonData);

    // ডাটা অ্যাক্সেস করা
    std::cout << "নাম: " << jsonObject["name"] << "\n"
              << "বয়স: " << jsonObject["age"] << "\n"
              << "শহর: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**স্যাম্পল আউটপুট:**

```
নাম: John
বয়স: 30
শহর: New York
```

### JSON জেনারেট করা
JSON ডাটা তৈরি করাও সমান সোজা; আপনি কেবল একটি `nlohmann::json` অবজেক্টে মান অ্যাসাইন করেন।

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // একটি JSON অবজেক্ট তৈরি করা
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // JSON অবজেক্ট থেকে স্ট্রিং কনভার্ট করে প্রিন্ট করা
    std::string jsonString = jsonObject.dump(4); // প্রিটি-প্রিন্টিং এর জন্য আর্গুমেন্ট 4
    std::cout << jsonString << std::endl;

    return 0;
}
```

**স্যাম্পল আউটপুট:**

```
{
    "নাম": "Jane",
    "বয়স": 25,
    "শহর": "Los Angeles"
}
```

এই উদাহরণগুলি C++ এ `nlohmann/json` লাইব্রেরি ব্যবহার করে JSON এর সাথে কাজ করার মূল কার্যকারিতা দেখায়। এই বেসিক্স গুলি দিয়ে, আপনি কনফিগুরেশন ফাইল থেকে নেটওয়ার্কেড অ্যাপ্লিকেশনে ডাটা আদান-প্রদানের জন্য বিভিন্ন প্রয়োগের জন্য JSON পার্স এবং জেনারেট করতে পারবেন।
