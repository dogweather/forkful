---
title:                "স্ট্রিং জোড়া দেওয়া"
date:                  2024-03-17T17:46:28.972266-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
স্ট্রিং সংযুক্তকরণ হল দুই বা ততোধিক স্ট্রিংকে শেষে শেষে জুড়ে দেওয়া। প্রোগ্রামাররা বাক্য গঠন, বার্তা তৈরি অথবা ইনপুট ডেটা প্রসেসিং বা প্রদর্শনের জন্য এটি করে থাকেন।

## কীভাবে:
C++ এ, আমাদের কাছে স্ট্রিং সংযুক্তিতে কয়েকটি উপায় আছে। এখানে `std::string` এবং প্লাস (`+`) অপারেটর ব্যবহার করে একটি উদাহরণ দেওয়া হল:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Hello, ";
    std::string world = "World!";
    
    std::string greeting = hello + world;
    
    std::cout << greeting << std::endl; // আউটপুট: Hello, World!
    return 0;
}
```

দ্রুত এবং সহজ, তাই না? কিন্তু, আমরা `append()` ব্যবহার করতে পারি:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Hello, ";
    hello.append("World!");
    
    std::cout << hello << std::endl; // আউটপুট: Hello, World!
    return 0;
}
```

অথবা আপনি চাইলে `operator+=` ব্যবহার করতে পারেন:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Hello, ";
    hello += "World!";
    
    std::cout << hello << std::endl; // আউটপুট: Hello, World!
    return 0;
}
```

## গভীরে ডুব
ঐতিহাসিকভাবে, C++ সি ভাষা থেকে উঠে এসেছে, যেখানে ক্যারেক্টার অ্যারে এবং `strcat()` ফাংশনের মতো ফাংশন স্ট্রিং কাজে ব্যবহৃত হতো। এটি ছিল আরও জটিল এবং ভুলের সম্ভাবনা ছিল বেশি।

আধুনিক C++ এ `std::string` দিয়ে পরিবেশে উন্নতি আনা হয়েছে। এটি নিরাপদ, পড়ার জন্য সহজ এবং আপনাকে বিকল্প প্রদান করে। যদি `std::string` আপনার পছন্দের না হয়, তবে ফরম্যাটিং ফ্যানের জন্য `std::stringstream` অথবা এমনকি C++20 থেকে `std::format` রয়েছে।

অন্তর্নিহিতভাবে, স্ট্রিং সংযুক্তকরণ মেমোরি বরাদ্দ এবং কপি করার প্রক্রিয়া জড়িত। অযত্নে করা হলে, এটি আপনার প্রোগ্রামের কর্মক্ষমতার উপর একটি ইট হিসেবে প্রভাব ফেলতে পারে। স্মার্ট পয়েন্টার এবং মুভ সেমান্টিক্স এখানে কিছু ব্যথা হ্রাস করে।

বিকল্প সম্পর্কে ভুলবেন না - Boost লাইব্রেরি বা আধুনিক C++ এ শূন্য-কপি অপারেশনের জন্য `std::string_view` দিয়ে UTF-8 হ্যান্ডেলিং।

## দেখুন এছাড়াও
- `std::string` এর জন্য C++ রেফারেন্স: https://cplusplus.com/reference/string/string/
- C++ প্রোগ্রামিং ভাষার জন্য C++ ওয়ার্কিং ড্রাফট, স্ট্যান্ডার্ড: http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2020/n4861.pdf
- `std::format` সম্পর্কে আরও জানুন: https://en.cppreference.com/w/cpp/utility/format
- Boost লাইব্রেরি ডকুমেন্টেশন: https://www.boost.org/doc/libs/1_75_0/libs/string_algo/doc/html/index.html
