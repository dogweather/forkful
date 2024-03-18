---
title:                "স্ট্রিং এর প্রথম অক্ষর বড় হাতের করা"
date:                  2024-03-17T17:45:41.545194-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

একটি স্ট্রিং ক্যাপিটালাইজ করা মানে স্ট্রিংয়ের প্রতিটি শব্দের প্রারম্ভিক অক্ষরকে যদি তা ছোট হাতের অক্ষরে থাকে তবে তাকে বড় হাতের অক্ষরে রূপান্তর করা, এবং বাকি অক্ষরগুলি অপরিবর্তিত রাখা। প্রায়ই প্রোগ্রামাররা আউটপুট ফরম্যাটিং, ব্যবহারকারীর ইনপুট বা ডাটা প্রক্রিয়াকরণের জন্য এই কাজটি করে থাকেন যাতে টেক্সটের উপস্থাপন বা প্রক্রিয়াজাতকরণে সামঞ্জস্য নিশ্চিত করা যায়, বিশেষ করে ব্যবহারকারীর ইন্টারফেস বা ডাটা স্বাভাবিকীকরণ কাজে।

## কিভাবে:
C++-এ, আপনি স্ট্যান্ডার্ড লাইব্রেরি ব্যবহার করে একটি স্ট্রিং ক্যাপিটালাইজ করতে পারেন, তৃতীয়-পক্ষের লাইব্রেরিগুলির প্রয়োজন ছাড়াই। তবে, আরও জটিল বা নির্দিষ্ট ক্যাপিটালাইজেশন আচরণের জন্য, Boost মতো লাইব্রেরিগুলি বেশ সহায়ক হতে পারে। নীচে উভয় পদ্ধতি দেখানো হয়েছে।

### স্ট্যান্ডার্ড C++ লাইব্রেরি ব্যবহার করে:

```cpp
#include <iostream>
#include <cctype> // std::tolower এবং std::toupper এর জন্য
#include <string>

std::string capitalizeString(const std::string& input) {
    std::string result;
    bool capitalizeNext = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        }
        result += ch;
    }

    return result;
}

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // আউটপুট: "Hello World From C++"
}
```

### Boost লাইব্রেরি ব্যবহার করে:

আরও উন্নত স্ট্রিং ম্যানিপুলেশনের জন্য, যেমন স্থানীয়ভিত্তিক ক্যাপিটালাইজেশন, আপনি Boost String Algo লাইব্রেরিটি ব্যবহার করতে চাইতে পারেন।

প্রথমে, নিশ্চিত করুন আপনার প্রকল্পে Boost লাইব্রেরি ইনস্টল এবং কনফিগার করা আছে। তারপর আপনি নীচে দেখানো হিসাবে প্রয়োজনীয় হেডারগুলি অন্তর্ভুক্ত করে এর বৈশিষ্ট্যাবলী ব্যবহার করতে পারেন।

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // প্রতিটি শব্দের প্রথম অক্ষর ক্যাপিটালাইজ করুন
    boost::algorithm::to_lower(capitalizedText); // নিশ্চিত করে স্ট্রিং ছোট হাতের অক্ষরে আছে
    capitalizedText[0] = std::toupper(capitalizedText[0]); // প্রথম অক্ষর ক্যাপিটালাইজ করুন

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // একটি স্পেসের পরে ক্যাপিটালাইজ করুন
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // আউটপুট: "Hello World From C++"
}
```

এই ক্ষেত্রে, Boost কিছু স্ট্রিং ম্যানিপুলেশন কাজকে সহজ করে দেয়, তবে প্রকৃত ক্যাপিটালাইজেশনের জন্য এখনও একটি ব্যক্তিগত পদ্ধতি প্রয়োজন হয় যেহেতু এটি মূলত রূপান্তর এবং কেস রূপান্তর সরঞ্জামগুলি অফার করে।
