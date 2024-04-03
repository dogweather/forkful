---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:37.570238-06:00
description: "C \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC JSON (\u099C\u09BE\u09AD\u09BE\
  \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u0985\u09AC\u099C\u09C7\u0995\
  \u09CD\u099F \u09A8\u09CB\u099F\u09C7\u09B6\u09A8) \u098F\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\
  \ JSON \u09A1\u09C7\u099F\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BE\u0995\u099A\
  \u09BE\u09B0 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE, \u099C\u09C7\u09A8\
  \u09BE\u09B0\u09C7\u099F \u0995\u09B0\u09BE, \u098F\u09AC\u0982 \u09AE\u09CD\u09AF\
  \u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\
  \u09BF\u2026"
lastmod: '2024-03-17T18:47:44.565455-06:00'
model: gpt-4-0125-preview
summary: "C \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC JSON (\u099C\u09BE\u09AD\u09BE\u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u0985\u09AC\u099C\u09C7\u0995\u09CD\
  \u099F \u09A8\u09CB\u099F\u09C7\u09B6\u09A8) \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 JSON\
  \ \u09A1\u09C7\u099F\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BE\u0995\u099A\u09BE\
  \u09B0 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE, \u099C\u09C7\u09A8\u09BE\
  \u09B0\u09C7\u099F \u0995\u09B0\u09BE, \u098F\u09AC\u0982 \u09AE\u09CD\u09AF\u09BE\
  \u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u0993\u09AF\u09BC\u09C7\u09AC\
  \ \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\u09B8, \u09A1\u09C7\u099F\u09BE \u09B8\u09CD\
  \u099F\u09CB\u09B0\u09C7\u099C, \u0985\u09A5\u09AC\u09BE \u0995\u09A8\u09AB\u09BF\
  \u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2\u09B8\u09AE\u09C2\u09B9\
  \u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AF\u09CB\u0997\u09BE\u09AF\u09CB\u0997\
  \ \u09B8\u09CD\u09A5\u09BE\u09AA\u09A8, \u09B9\u09BE\u09B2\u0995\u09BE \u098F\u09AC\
  \u0982 \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09CD\u09AF \u09AB\u09B0\u09AE\
  \u09CD\u09AF\u09BE\u099F\u09C7\u0964."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কিভাবে:
C ভাষায় JSON এর সাথে কাজ করতে, আপনাকে সাধারণত `jansson` অথবা `json-c` এর মতো লাইব্রেরি ব্যবহার করতে হবে যেহেতু C ভাষায় JSON এর জন্য কোনো বিল্ট-ইন সাপোর্ট নেই। এখানে, আমরা `jansson` লাইব্রেরিতে মনোনিবেশ করব কারণ এর ব্যবহার সহজ এবং নিয়মিত রক্ষণাবেক্ষণ করা হয়। প্রথমে, লাইব্রেরিটি ইনস্টল করুন (যেমন, উবুন্টুতে `apt` প্যাকেজ ম্যানেজার ব্যবহার করে: `sudo apt-get install libjansson-dev`).

JSON স্ট্রিং পার্স করা এবং এর কন্টেন্টস অ্যাক্সেস করা দেখা যাক:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "error: on line %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("নাম: %s\nবয়স: %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

নমুনা আউটপুট:
```
নাম: John Doe
বয়স: 30
```

পরবর্তীতে, একটা JSON অবজেক্ট তৈরি এবং আউটপুট দেখানো হচ্ছে:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    json_t *root = json_object();
    json_object_set_new(root, "name", json_string("Jane Doe"));
    json_object_set_new(root, "age", json_integer(25));
    
    char *json_dump = json_dumps(root, JSON_ENCODE_ANY);
    printf("%s\n", json_dump);
    
    free(json_dump);
    json_decref(root);
    return 0;
}
```

নমুনা আউটপুট:
```
{"name": "Jane Doe", "age": 25}
```

এই উদাহরণগুলি দেখায় যে JSON স্ট্রিং লোড করা, এর মান আনপ্যাক করা, নতুন একটি JSON অবজেক্ট তৈরি করা, এবং তারপর এটিকে একটি স্ট্রিং হিসেবে আউটপুট করা জেসনের বেসিকগুলি।

## গভীর ডুব দেওয়া
ওয়েবে JSON কে ডেটা ইন্টারচেঞ্জের প্রাথমিক ফরম্যাট হিসেবে গ্রহণের ফলে C ভাষায় JSON এর সাথে কাজ করার প্রয়োজন উদ্ভূ�্ব হয়েছে। JSON এর সরলতা এবং দক্ষতা এটিকে দ্রুত XML এর চেয়ে এগিয়ে নিয়ে যায়, যদিও C ভাষার JSON ম্যানিপুলেশনের জন্য প্রাথমিকভাবে সরাসরি সাপোর্টের অভাব ছিল। প্রথম প্রথম, সমাধানগুলি ম্যানুয়াল স্ট্রিং ম্যানিপুলেশনে নির্ভর করত - যা ত্রুটিপূর্ণ এবং অকার্যকর। `jansson` এবং `json-c` এর মতো লাইব্রেরিগুলি এই ঘাটতি পূরণ করে, JSON পার্সিং, নির্মাণ, এবং সিরিয়ালাইজেশনের জন্য শক্তিশালী APIs সরবরাহ করে।

`jansson` সারল্য এবং ব্যবহারে সুবিধার সমর্থন করলেও, `json-c` হয়তো সেইসব লোকেদের আকর্ষণ করবে যারা আরও বিস্তৃত ফিচার সেট খুঁজে থাকেন। যাইহোক, C++ ভাষার পার্সিং লাইব্রেরিগুলির মতো বিকল্পগুলি আরও উন্নত অ্যাবস্ট্রাকশন অফার করে, কারণ সেই ভাষা�্য আরও জটিল ডেটা স্ট্রাকচার এবং স্ট্যান্ডার্ড লাইব্রেরি সাপোর্ট রয়েছে। তবে, যখন এমবেডেড সিস্টেমগুলিতে বা বিদ্যমান C লাইব্রেরিগুলি�্য ইন্টারফেসিং করার সময় C ভাষা পছন্দ বা আবশ্যক হয়, তখন `jansson` বা `json-c` ব্যবহার করা অপরিহার্য হয়ে ওঠে।

এটাও উল্লেখ্য যে C ভাষায় JSON এর সাথে কাজ করা মেমোরি ম্যানেজমেন্টের গভীর বোঝাপড়া জড়িত, কারণ এই লাইব্রেরিগুলি প্রায়শই স্পষ্টভাবে ডিআলোকেট করা আবশ্যক ডায়নামিকালি আলোকিত অবজেক্টগুলি ফিরিয়ে আসে। এই চ্যালেঞ্জটি প্রোগ্রামারদের কাছে সুবিধার সাথে মেমোরি লিক প্রতিরোধের দায়িত্বের ভারসাম্য রাখার জন্য একটি কী দিক তৈরি করে, C কোড দক্ষতার সাথে তৈরির একটি অপরিহার্য দিক।
