---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:40.040479-06:00
description: null
lastmod: '2024-04-05T21:53:52.980956-06:00'
model: gpt-4-0125-preview
summary: ''
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কিভাবে:


### C++ স্ট্যান্ডার্ড লাইব্রেরি ব্যবহার করে CSV ফাইল পড়া:
```cpp
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

int main() {
    std::ifstream file("data.csv");
    std::string line;
    
    while (std::getline(file, line)) {
        std::stringstream lineStream(line);
        std::string cell;
        std::vector<std::string> parsedRow;
        
        while (std::getline(lineStream, cell, ',')) {
            parsedRow.push_back(cell);
        }
        
        // এখানে parsedRow প্রক্রিয়াজাত করুন
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### একটি CSV ফাইলে লেখা:
```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> ডেটা = {
        {"নাম", "বয়স", "শহর"},
        {"জন ডো", "29", "নিউ ইয়র্ক"},
        {"জেন স্মিথ", "34", "লস এঞ্জেলেস"}
    };
    
    for (const auto& সারি : ডেটা) {
        for (size_t i = 0; i < সারি.size(); i++) {
            file << সারি[i];
            if (i < সারি.size() - 1) file << ",";
        }
        file << "\n";
    }
    
    return 0;
}
```

### একটি তৃতীয় পক্ষের লাইব্রেরি ব্যবহার: `csv2`:
যদিও C++ স্ট্যান্ডার্ড লাইব্রেরি ফাইল এবং স্ট্রিং নিয়ে কাজ করার জন্য মৌলিক সরঞ্জাম প্রদান করে, তৃতীয় পক্ষের লাইব্রেরি ব্যবহার করে CSV প্রসেসিং সহজ করা যেতে পারে। এমন একটি লাইব্রেরি হল `csv2`, যা এর সহজ ব্যবহার এবং কার্যকারিতা জন্য পরিচিত।

- ইনস্টলেশন: সাধারণত কোনানের মতো প্যাকেজ ম্যানেজারগুলির মাধ্যমে অথবা সরাসরি এর GitHub রিপোজিটরি থেকে ইনস্টল করা হয়।

`csv2` ব্যবহার করে CSV ফাইল পড়ার উদাহরণ:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto header = csv.header();
        for (const auto row : csv) {
            for (const auto cell : row) {
                std::cout << cell.second << "\t"; // প্রতিটি কোষের মান প্রিন্ট করুন
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

পড়ার অপারেশনের জন্য নমুনা আউটপুট এইরকম দেখাবে (ধরে নেওয়া হয়েছে একটি সাধারণ তিন কলামের CSV ফাইল):

```
জন    29    নিউ ইয়র্ক    
জেন    34    লস এঞ্জেলেস
```

এই উদাহরণগুলি C++ এ CSV অপারেশনের মৌলিক ধারণা কভার করার জন্য লক্ষ্য করা হয়েছে। বড় ফাইলগুলি বা জটিল ডেটা ট্রান্সফরমেশনের মতো আরও জটিল পরিস্থিতি সম্পর্কে জানতে বিশেষায়িত লাইব্রেরি বা সরঞ্জামে আরও গবেষণা করা দরকার হতে পারে।
