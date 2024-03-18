---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:04.861927-06:00
description: "JSON (JavaScript Object Notation) \u09B9\u09B2\u09CB \u09A1\u09BE\u099F\
  \u09BE \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3 \u0993 \u09AA\u09B0\u09BF\u09AC\
  \u09B9\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B9\
  \u09BE\u09B2\u09CD\u0995\u09BE \u09AC\u09BF\u09A8\u09CD\u09AF\u09BE\u09B8, \u09AF\
  \u09BE \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0 \u098F\u09AC\u0982 \u0993\u09AF\
  \u09BC\u09C7\u09AC \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\
  \u09A8\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09A1\u09BE\u099F\u09BE \u0986\
  \u09A6\u09BE\u09A8-\u09AA\u09CD\u09B0\u09A6\u09BE\u09A8\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF\u2026"
lastmod: '2024-03-17T18:47:44.386699-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u09B9\u09B2\u09CB \u09A1\u09BE\u099F\u09BE\
  \ \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3 \u0993 \u09AA\u09B0\u09BF\u09AC\u09B9\
  \u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B9\u09BE\
  \u09B2\u09CD\u0995\u09BE \u09AC\u09BF\u09A8\u09CD\u09AF\u09BE\u09B8, \u09AF\u09BE\
  \ \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0 \u098F\u09AC\u0982 \u0993\u09AF\u09BC\
  \u09C7\u09AC \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\
  \u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09A1\u09BE\u099F\u09BE \u0986\u09A6\
  \u09BE\u09A8-\u09AA\u09CD\u09B0\u09A6\u09BE\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF\u2026"
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

JSON (JavaScript Object Notation) হলো ডাটা সংরক্ষণ ও পরিবহনের জন্য একটি হাল্কা বিন্যাস, যা সার্ভার এবং ওয়েব অ্যাপ্লিকেশনের মধ্যে ডাটা আদান-প্রদানের জন্য একটি চমৎকার মাধ্যম হিসাবে কাজ করে। প্রোগ্রামাররা JSON ব্যবহার করে এর মানুষের দ্বারা সহজে পড়ার ক্ষমতা এবং মেশিনের দ্বারা সরলভাবে পার্সিংযোগ্যতা এর জন্য, বিশেষ করে যখন ইন্টারনেটের মাধ্যমে ডাটা আদান-প্রদান বা কনফিগুরেশন সেটিংস নিয়ে কাজ করা হয়।

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
