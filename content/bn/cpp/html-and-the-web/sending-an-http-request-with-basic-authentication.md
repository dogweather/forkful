---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:18:34.176894-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ `CURL` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 C++ \u098F \u098F\u0995\u099F\u09BF\
  \ \u09AC\u09C7\u09B8\u09BF\u0995 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\
  \u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0964 \u09A1\u09C1\u09AC \u09A6\u09C7\u0993\
  \u09AF\u09BC\u09BE\u09B0 \u0986\u0997\u09C7, \u0986\u09AA\u09A8\u09BF `libcurl`\
  \ \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09C7 \u09A8\u09BF\u09B6\u09CD\
  \u099A\u09BF\u09A4 \u0995\u09B0\u09C1\u09A8\u0964."
lastmod: '2024-03-17T18:47:44.365012-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 `CURL` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ C++ \u098F \u098F\u0995\u099F\u09BF \u09AC\u09C7\u09B8\u09BF\u0995 \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0964\
  \ \u09A1\u09C1\u09AC \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u0986\u0997\u09C7\
  , \u0986\u09AA\u09A8\u09BF `libcurl` \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\
  \u09B0\u09C7 \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09C1\u09A8\
  \u0964."
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
weight: 45
---

## কিভাবে:
এখানে `CURL` লাইব্রেরি ব্যবহার করে C++ এ একটি বেসিক উদাহরণ দেওয়া হল। ডুব দেওয়ার আগে, আপনি `libcurl` ইনস্টল করে নিশ্চিত করুন।

```C++
#include <iostream>
#include <curl/curl.h>

// curl দ্বারা প্রাপ্ত ডাটা পরিচালনা করার জন্য সাধারণ কলব্যাক ফাংশন
static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://yourapi.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "pass");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        
        // অনুরোধটি সম্পাদন করুন এবং ত্রুটিগুলির জন্য পরীক্ষা করুন
        res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        } else {
            std::cout << readBuffer << std::endl;
        }
        
        // পরিষ্কার করা
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

আপনি ধরে নেন কোন ত্রুটি ঘটেনি, তাহলে সার্ভার থেকে একটি সাড়া কনসোলে মুদ্রিত হবে।

## গভীর চিন্তা
বেসিক অথেনটিকেশন হল পুরানো স্কুলের, HTTP-এর প্রারম্ভিক দিনগুলিতে ডেটিং করা। এখন, শিল্পের পছন্দ OAuth এবং টোকেনের মতো আরও নিরাপদ পদ্ধতিগুলির দিকে ঝুঁকে আছে। তারপরও, প্রায়শই অভ্যন্তরীণ বা সাধারণ সিস্টেমগুলির জন্য বেসিক অথ ব্যবহার করা হয়, যেখানে ভারী নিরাপত্তা স্তরগুলি অতিরিক্ত ভারী হতে পারে।

ভিতরের দিকে, আপনার ব্যবহারকারীর নাম এবং পাসওয়ার্ড বেস64-এ কোডেক করে এবং HTTP হেডারে রাখা হয়। এটি সাধারণ কিন্তু এটি HTTPS ব্যতীত অনিরাপদ কারণ বেস64 সহজে বিপরীতযোগ্য—HTTPS একটি অবশ্য প্রয়োজন।

যদি `libcurl` আপনার পছন্দ না হয়, `cpp-httplib` লাইব্রেরির মতো বিকল্পগুলি বিবেচনা করুন, অথবা `Boost.Beast` এর সাথে আরও হাতে-কলমে একটি পদ্ধতি অনুসরণ করুন।

## আরও দেখুন
- [libcurl](https://curl.se/libcurl/)
- [cpp-httplib GitHub রিপোজিটরি](https://github.com/yhirose/cpp-httplib)
- [Boost.Beast ডকুমেন্টেশন](https://www.boost.org/doc/libs/master/libs/beast/doc/html/index.html)
