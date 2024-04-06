---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:26:13.638803-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0986\u09B0\u09CD\u09A6\u09C1\
  \u0987\u09A8\u09CB\u09B0 \u09AE\u09BE\u09A8 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF\u09A4\u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF regex \u098F\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09B8\
  \u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u09A8\u09C7\u0987\u0964 \u09A4\u09AC\u09C7\
  , \u0986\u09AA\u09A8\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u09AA\u09CD\u09AF\
  \u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u099C\u09A8\u09CD\u09AF \u09AE\u09CC\u09B2\
  \u09BF\u0995 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AB\u09BE\u0982\u09B6\
  \u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 regex-\u098F\
  \u09B0 \u09AE\u09A4\u2026"
lastmod: '2024-04-05T21:53:52.840961-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B0\u09CD\u09A6\u09C1\u0987\u09A8\u09CB\u09B0 \u09AE\u09BE\u09A8\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09A4\u09C7 \u09B8\u09B0\
  \u09BE\u09B8\u09B0\u09BF regex \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\
  \u09B2\u09CD\u099F-\u0987\u09A8 \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u09A8\
  \u09C7\u0987\u0964 \u09A4\u09AC\u09C7, \u0986\u09AA\u09A8\u09BF \u09B8\u09BE\u09A7\
  \u09BE\u09B0\u09A3 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u099C\
  \u09A8\u09CD\u09AF \u09AE\u09CC\u09B2\u09BF\u0995 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 regex-\u098F\u09B0 \u09AE\u09A4 \u0995\u09BE\u09B0\u09CD\
  \u09AF\u0995\u09BE\u09B0\u09BF\u09A4\u09BE \u0985\u09B0\u09CD\u099C\u09A8 \u0995\
  \u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8, \u0985\u09A5\u09AC\u09BE \u099C\
  \u099F\u09BF\u09B2 \u099A\u09BE\u09B9\u09BF\u09A6\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF `regex` \u098F\u09B0 \u09AE\u09A4 \u09A4\u09C3\u09A4\u09C0\u09DF-\u09AA\u0995\
  \u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \ \u0987\u09A8\u09CD\u099F\u09BF\u0997\u09CD\u09B0\u09C7\u099F \u0995\u09B0\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964."
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

## কিভাবে:
আর্দুইনোর মান লাইব্রেরিতে সরাসরি regex এর জন্য বিল্ট-ইন সাপোর্ট নেই। তবে, আপনি সাধারণ প্যাটার্ন জন্য মৌলিক স্ট্রিং ফাংশন ব্যবহার করে regex-এর মত কার্যকারিতা অর্জন করতে পারেন, অথবা জটিল চাহিদার জন্য `regex` এর মত তৃতীয়-পক্ষের লাইব্রেরি ইন্টিগ্রেট করতে পারেন।

### Regex ছাড়া বেসিক স্ট্রিং মিলান
সাবস্ট্রিং খুঁজে পাওয়ার মতো প্রাথমিক চাহিদার জন্য, আপনি `String.indexOf()` ফাংশন ব্যবহার করতে পারেন:
```cpp
String data = "Sensor value: 12345";
int index = data.indexOf("value:");
if (index != -1) {
  String value = data.substring(index + 6).trim();
  Serial.println(value); // আউটপুট: 12345
}
```

### Regex ব্যবহারের জন্য তৃতীয়-পক্ষের লাইব্রেরি
জটিল প্যাটার্ন হ্যান্ডল করার জন্য, আপনি `regex` এর মতো একটি লাইব্রেরি বিবেচনা করতে পারেন। লাইব্রেরিটি ইনস্টল করার পর, আপনি এটি নিম্নরূপে ব্যবহার করতে পারেন:

1. **ইনস্টলেশন**: `regex` লাইব্রেরিটি হয়তো আর্দুইনো লাইব্রেরি ম্যানেজারে সহজে উপলব্ধ নয়, তাই আপনাকে একটি বিশ্বস্ত সোর্স থেকে ম্যানুয়ালি ইনস্টল করতে হবে এবং আর্দুইনো লাইব্রেরি ফোল্ডারে যোগ করতে হবে।

2. **উদাহরণ ব্যবহার**:
ধরে নেওয়া যাক, লাইব্রেরিটি স্ট্যান্ডার্ড regex বাস্তবায়নের মতো কার্যকারিতা সরবরাহ করে, তাহলে আপনি এটি নিম্নরূপে ব্যবহার করতে পারেন:

```cpp
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // সিরিয়ালের জন্য অপেক্ষা করুন
  
  regex_t reg;
  const char* pattern = "[0-9]+"; // সংখ্যার একটি ধারাবাহিক ম্যাচ করে
  regcomp(&reg, pattern, REG_EXTENDED);
  
  const char* test_str = "Sensor value: 12345";
  
  regmatch_t matches[1];
  if (regexec(&reg, test_str, 1, matches, 0) == 0) {
    // মিলে যাওয়া অংশটি এক্সট্র্যাক্ট এবং প্রিন্ট করুন
    int start = matches[0].rm_so;
    int end = matches[0].rm_eo;
    char match[end-start+1];
    strncpy(match, test_str + start, end-start);
    match[end-start] = '\0';
    
    Serial.print("ম্যাচ পাওয়া গেছে: ");
    Serial.println(match); // আউটপুট: 12345
  } else {
    Serial.println("কোন ম্যাচ পাওয়া যায়নি");
  }
  
  regfree(&reg); // regex এর জন্য বরাদ্দ করা মেমোরিটি মুক্ত করুন
}

void loop() {
  // আপনার মুখ্য কোড এখানে লিখুন, বারবার চালানোর জন্য:
}
```

**নোট**: এখানে ব্যবহৃত সিনট্যাক্স এবং নির্দিষ্ট ফাংশনগুলি উদাহরণের জন্য এবং আপনি যে `regex` লাইব্রেরিটি বেছে নেবেন তার প্রকৃত বাস্তবায়নের বিস্তারিত ভিত্তিতে ভিন্ন হতে পারে। সঠিক এবং সর্বশেষ তথ্যের জন্য সবসময় লাইব্রেরির ডকুমেন্টেশনে পরামর্শ করুন।
