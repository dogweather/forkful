---
title:                "টেক্সট অনুসন্ধান এবং প্রতিস্থাপন"
date:                  2024-03-17T18:15:52.299448-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
টেক্সট অনুসন্ধান ও প্রতিস্থাপন হ'ল একটি বড় স্ট্রিংয়ের মধ্যে নির্দিষ্ট স্ট্রিংগুলি খুঁজে বের করা এবং তাদেরকে অন্য কিছু দিয়ে বদলে দেওয়া। প্রোগ্রামাররা এটি ভেরিয়েবল নাম আপডেট করা, ডাটা মডিফাই করা, বা একাধিক ফাইলে অটোমেটেড এডিট করার মত কাজে ব্যবহার করে।

## কিভাবে:
C++ টেক্সট অনুসন্ধান ও প্রতিস্থাপন করার একাধিক উপায় অফার করে। নীচে `std::string::find` এবং `std::string::replace` ব্যবহার করে একটি উদাহরণ দেওয়া হলো।

```cpp
#include <iostream>
#include <string>

int main() {
    std::string myText = "The quick brown fox jumps over the lazy dog.";
    std::string wordToSearch = "lazy";
    std::string replacement = "energetic";

    size_t pos = myText.find(wordToSearch);
    
    if (pos != std::string::npos) {
        myText.replace(pos, wordToSearch.length(), replacement);
    }

    std::cout << myText << std::endl; // আউটপুট: The quick brown fox jumps over the energetic dog.
    return 0;
}
```

## গভীর ডুব
`find` এবং `replace` ফাংশনগুলি C++'র `std::string` ক্লাসের অংশ বহুযুগ ধরে রয়েছে, যা তাদেরকে টেক্সট নিয়ন্ত্রণের জন্য একটি মৌলিক কিন্তু শক্তিশালী মাধ্যম করে তোলে। `std::string`-এর আগে C প্রোগ্রামাররা অনুরূপ কাজের জন্য ক্যারেক্টার অ্যারে এবং C স্ট্যান্ডার্ড লাইব্রেরি থেকে `strstr` এবং `strcpy` মত ফাংশনগুলি ব্যবহার করত, যা আরও ভুল হওয়ার ঝুঁকি ছিল এবং ম্যানুয়াল মেমোরি ম্যানেজমেন্ট প্রয়োজন হত।

বিকল্প হিসেবে, `std::regex` মতো অন্যান্য স্ট্যান্ডার্ড লাইব্রেরি কম্পোনেন্ট জটিল অনুসন্ধান এবং প্রতিস্থাপন পরিস্থিতিগুলির জন্য প্যাটার্ন-ভিত্তিক টেক্সট ম্যানিপুলেশন ক্ষমতা সরবরাহ করে। বুস্টের মতো তৃতীয় পক্ষের লাইব্রেরিগুলি আরও উন্নত টেক্সট প্রসেসিং বিকল্প অফার করে।

অভ্যন্তরীণভাবে, অনুসন্ধান এবং প্রতিস্থাপন মিল অনুসারে অক্ষরের ক্রম খুঁজে পেতে এবং তারপর স্ট্রিংয়ের বিষয়বস্তু যথাযথভাবে পরিবর্তন করতে এলগরিদম ব্যবহার করে। এই অপারেশনগুলির দক্ষতা অনুসন্ধানের প্যাটার্নের জটিলতা এবং বাস্তবায়নের উপর নির্ভর করে বিভিন্ন হতে পারে।

## আরও দেখুন
- `std::string::find` এর জন্য C++ রেফারেন্স: https://en.cppreference.com/w/cpp/string/basic_string/find
- `std::string::replace` এর জন্য C++ রেফারেন্স: https://en.cppreference.com/w/cpp/string/basic_string/replace
- রেগুলার এক্সপ্রেশনস `std::regex` এর জন্য C++ রেফারেন্স: https://en.cppreference.com/w/cpp/regex
- বুস্ট স্ট্রিং অ্যালগোরিদম লাইব্রেরি: https://www.boost.org/doc/libs/release/libs/algorithm/string/
