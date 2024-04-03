---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:31.711614-06:00
description: "TOML (Tom's Obvious, Minimal Language) \u098F\u0995\u099F\u09BF \u0995\
  \u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2\u09C7\
  \u09B0 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u09AF\u09BE \u098F\u09B0 \u09B8\
  \u09CD\u09AA\u09B7\u09CD\u099F \u09B8\u09C7\u09AE\u09BE\u09A8\u09CD\u099F\u09BF\u0995\
  \u09B8 \u098F\u09B0 \u0995\u09BE\u09B0\u09A3\u09C7 \u09AA\u09A1\u09BC\u09BE \u09B8\
  \u09B9\u099C\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\
  \u09C7 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.567728-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) \u098F\u0995\u099F\u09BF \u0995\u09A8\
  \u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2\u09C7\u09B0\
  \ \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u09AF\u09BE \u098F\u09B0 \u09B8\u09CD\
  \u09AA\u09B7\u09CD\u099F \u09B8\u09C7\u09AE\u09BE\u09A8\u09CD\u099F\u09BF\u0995\u09B8\
  \ \u098F\u09B0 \u0995\u09BE\u09B0\u09A3\u09C7 \u09AA\u09A1\u09BC\u09BE \u09B8\u09B9\
  \u099C\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\
  \ \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\
  \u09B2\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0995\u09BE\u09B0\u09A3 \u098F\u09B0\
  \ \u09B8\u09BE\u09A6\u09BE\u09B8\u09BF\u09A7\u09BE \u098F\u09AC\u0982 \u09AE\u09BE\
  \u09A8\u09AC-\u09AA\u09BE\u09A0\u09CD\u09AF\u09A4\u09BE \u098F\u099F\u09BF\u0995\
  \u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09AA\u09B0\u09BF\
  \u09B8\u09CD\u09A5\u09BF\u09A4\u09BF\u09A4\u09C7 XML \u0985\u09A5\u09AC\u09BE JSON\
  \ \u098F\u09B0 \u09AE\u09A4\u09CB \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09C7\
  \u09B0 \u09A4\u09C1\u09B2\u09A8\u09BE\u09AF\u09BC \u098F\u0995\u099F\u09BF \u099A\
  \u09AE\u09CE\u0995\u09BE\u09B0 \u09AA\u099B\u09A8\u09CD\u09A6 \u0995\u09B0\u09C7\
  \ \u09A4\u09CB\u09B2\u09C7\u0964."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কি এবং কেন?

TOML (Tom's Obvious, Minimal Language) একটি কনফিগারেশন ফাইলের ফরম্যাট যা এর স্পষ্ট সেমান্টিকস এর কারণে পড়া সহজ। প্রোগ্রামাররা অ্যাপ্লিকেশনে কনফিগারেশন ফাইলের জন্য এটি ব্যবহার করে কারণ এর সাদাসিধা এবং মানব-পাঠ্যতা এটিকে নির্দিষ্ট পরিস্থিতিতে XML অথবা JSON এর মতো ফরম্যাটের তুলনায় একটি চমৎকার পছন্দ করে তোলে।

## কিভাবে:

C তে TOML এর সাথে কাজ করতে, প্রথমে আপনার এমন একটি লাইব্রেরির প্রয়োজন যা TOML ফাইল পার্স করতে সক্ষম, কারণ C স্ট্যান্ডার্ড লাইব্রেরিতে এই ফাংশনালিটি অন্তর্ভুক্ত করা নেই। একটি জনপ্রিয় পছন্দ হল `tomlc99`, C99 এর জন্য একটি লাইটওয়েট TOML পার্সার। এখানে একটি সাধারণ TOML কনফিগ ফাইল পড়ার একটি দ্রুত গাইড দেওয়া হল:

প্রথমে, নিশ্চিত করুন আপনার `tomlc99` ইনস্টল করা আছে এবং আপনার প্রজেক্টে সঠিকভাবে লিঙ্ক করা আছে।

**স্যাম্পল TOML ফাইল (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**এই ফাইল পার্স করার জন্য C কোড:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("Cannot open file");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Error parsing file\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Database Server: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("Port %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**আউটপুট:**
```
Database Server: "192.168.1.1"
Port 0: 8001
Port 1: 8001
Port 2: 8002
```

## গভীরে ডুব:

TOML টম প্রেস্টন-ওয়ার্নার, GitHub-এর সহ-প্রতিষ্ঠাতা দ্বারা অন্যান্য কনফিগারেশন ফাইল ফরম্যাটের সীমাবদ্ধতাগুলির প্রতি সাড়া দিয়ে তৈরি করা হয়েছিল। এর লক্ষ্য হল মানুষ এবং কম্পিউটার উভয়ের জন্য পড়া এবং লেখা সোজা এবং অস্পষ্টতামুক্ত করা, জটিল পার্সিং নিয়মগুলির প্রয়োজন ছাড়াই। C ইকোসিস্টেমে, TOML Rust এর `serde_toml` অথবা Python এর `toml` এর মতো উচ্চস্তরীয় ভাষাগুলিতে যেমন একটি প্রথম শ্রেণীর নাগরিক নয়, যেখানে স্বদেশী সমর্থন সহ লাইব্রেরিগুলি আছে। বরং, C ডেভেলপারদের `tomlc99` এর মতো বাহ্যিক লাইব্রেরিগুলির উপর নির্ভর করতে হয়, তবে এটি C এর ন্যূনতমতা এবং পারফরম্যান্সের জোর দেওয়ার বিষয়ে স্বাভাবিক।

TOML এর স্পষ্টতা প্রশংসিত হলেও, একটি কনফিগারেশন ফাইল ফরম্যাট বেছে নেওয়ার সময় প্রজেক্টের প্রয়োজনগুলি বিবেচনা করা জরুরি। সেনারিওগুলিতে যেখানে আরো জটিল কাঠামো অথবা ওয়েব API-এর সাথে মিথস্ক্রিয়া প্রয়োজন, JSON অথবা এমনকি YAML তাদের বৃদ্ধ জটিলতার সত্ত্বেও একটি ভাল ফিট অফার করতে পারে। TOML পঠনযোগ্যতা এবং সাদাসিধাতা যেখানে পরম গুরুত্বপূর্ণ, সেখানে ঝলমলে, না যেখানে সবচেয়ে উন্নত ডেটা কাঠামো প্রয়োজন।
