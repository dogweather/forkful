---
title:                "স্ট্রিং থেকে তারিখ পার্স করা"
date:                  2024-03-17T18:06:00.392394-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
স্ট্রিং থেকে তারিখ পার্সিং করা হল স্ট্রিং ফর্ম্যাটকে ব্যাখ্যা করে দিন, মাস এবং বছরের মতো তারিখের ঘটকগুলি নির্যাস করা। প্রোগ্রামাররা ব্যবহারকারীর ইনপুট নিয়ে কাজ করতে, ডেটা ফাইল পড়তে অথবা এমন এপিআইয়ের সাথে মিথস্ক্রিয়া করতে এটি করে থাকেন যা স্ট্রিং ফর্ম্যাটে তারিখ প্রেরণ করে। অ্যাপ্লিকেশনগুলিতে ডেটা প্রসেসিং, যাচাইকরণ, এবং তারিখের গাণিতিক কাজ সম্পাদনের জন্য এটি অপরিহার্য।

## কিভাবে:
আধুনিক C++ এ, তারিখ এবং সময়কে স্বাভাবিকভাবে হ্যান্ডেল করার জন্য আপনি `<chrono>` লাইব্রেরি ব্যবহার করতে পারেন, তবে এটি সরাসরি স্ট্রিং থেকে পার্সিং করা সমর্থন করে না যদি না আপনি আরও জটিল ফর্ম্যাটগুলির জন্য ম্যানুয়াল পার্সিং করেন। তবে, ISO 8601 তারিখের ফর্ম্যাটগুলি এবং সহজ কাস্টম ফর্ম্যাটগুলির জন্য, আপনি তারিখ পার্সিং কিভাবে সম্পাদন করতে পারেন তার একটি উদাহরণ এখানে দেওয়া হলো।

**`<chrono>` এবং `<sstream>` ব্যবহার করে:**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // ISO 8601 format
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Parsed date: " << parsed_date << std::endl;
    } else {
        std::cout << "Failed to parse date." << std::endl;
    }
    
    return 0;
}
```
নমুনা আউটপুট:
```
Parsed date: 2023-04-15
```

আরও জটিল ফর্ম্যাটগুলির ক্ষেত্রে অথবা পুরোনো C++ সংস্করণগুলি ব্যবহার করার সময়, `date.h` লাইব্রেরি (হাওয়ার্ড হিন্নান্টের তারিখের লাইব্রেরি) জনপ্রিয়। এটি ব্যবহার করে বিভিন্ন ফর্ম্যাট পার্স করার উপায় এখানে দেওয়া হলো:

**`date.h` লাইব্রেরি ব্যবহার করে:**
লাইব্রেরিটি ইনস্টল করা আছে তা নিশ্চিত করুন। এটি আপনি [এখানে](https://github.com/HowardHinnant/date) পাবেন।

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "April 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Parsed date: " << parsed_date << std::endl;
    } else {
        std::cout << "Failed to parse date from string." << std::endl;
    }

    return 0;
}
```
নমুনা আউটপুট (আপনার সিস্টেমের লোকেল এবং তারিখের সেটিংসের উপর নির্ভর করতে পারে):
```
Parsed date: 2023-04-15
```
