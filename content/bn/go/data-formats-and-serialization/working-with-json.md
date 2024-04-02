---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:44.716559-06:00
description: "Go \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC JSON (JavaScript Object Notation)\
  \ \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 Go \u09A1\u09BE\u099F\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BE\
  \u0995\u099A\u09BE\u09B0 \u098F\u09AC\u0982 JSON \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\
  \u09BE\u099F\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09A1\u09BE\u099F\u09BE\
  \ \u098F\u09A8\u0995\u09CB\u09A1 \u098F\u09AC\u0982 \u09A1\u09BF\u0995\u09CB\u09A1\
  \ \u0995\u09B0\u09BE\u0964 \u098F\u0987 \u0995\u09BE\u099C\u099F\u09BF \u0993\u09AF\
  \u09BC\u09C7\u09AC\u2026"
lastmod: '2024-03-17T18:47:43.499722-06:00'
model: gpt-4-0125-preview
summary: "Go \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC JSON (JavaScript Object Notation)\
  \ \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 Go \u09A1\u09BE\u099F\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BE\
  \u0995\u099A\u09BE\u09B0 \u098F\u09AC\u0982 JSON \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\
  \u09BE\u099F\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09A1\u09BE\u099F\u09BE\
  \ \u098F\u09A8\u0995\u09CB\u09A1 \u098F\u09AC\u0982 \u09A1\u09BF\u0995\u09CB\u09A1\
  \ \u0995\u09B0\u09BE\u0964 \u098F\u0987 \u0995\u09BE\u099C\u099F\u09BF \u0993\u09AF\
  \u09BC\u09C7\u09AC\u2026"
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কি এবং কেন?

Go ভাষায় JSON (JavaScript Object Notation) এর সাথে কাজ করা মানে Go ডাটা স্ট্রাকচার এবং JSON ফর্ম্যাটের মধ্যে ডাটা এনকোড এবং ডিকোড করা। এই কাজটি ওয়েব সার্ভিস এবং APIs এ খুবই সাধারণ, কেননা JSON হিসেবে কাজ করে একটি লাইটওয়েট, টেক্সট-বেসড, এবং ভাষা-নিরপেক্ষ ডাটা ইন্টারচেঞ্জ ফর্ম্যাট হিসেবে, যা বিভিন্ন প্রোগ্রামিং পরিবেশের মধ্যে সহজে ডাটা শেয়ারিং এনেবল করে।

## কিভাবে:

Go তে, `encoding/json` প্যাকেজ JSON ম্যানিপুলেশনের গেটওয়ে, যা Go ডাটা স্ট্রাকচারগুলিকে JSON এ রূপান্তর (মার্শালিং) এবং তার বিপরীতে (আনমার্শালিং) কনভার্ট করার যন্ত্রণাদি প্রদান করে। নিচে শুরু করার জন্য মৌলিক উদাহরণ দেওয়া হল:

### এনকোডিং (মার্শালিং)

Go struct কে JSON এ রূপান্তর করতে, আপনি `json.Marshal` ব্যবহার করতে পারেন। নিচের Go struct টি বিবেচনা করুন:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID        int      `json:"id"`
    Username  string   `json:"username"`
    Languages []string `json:"languages"`
}

func main() {
    user := User{1, "JohnDoe", []string{"Go", "JavaScript", "Python"}}
    userJSON, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(userJSON))
}
```

আউটপুট:

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### ডিকোডিং (আনমার্শালিং)

Go ডাটা স্ট্রাকচারে JSON পার্স করতে, `json.Unmarshal` ব্যবহার করুন:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

func main() {
    jsonStr := `{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}`
    var user User
    err := json.Unmarshal([]byte(jsonStr), &user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", user)
}
```

পূর্ববর্তী `User` struct হিসাবে, এই কোডটি JSON স্ট্রিংটিকে একটি User ইন্সট্যান্সে পার্স করে।

আউটপুট:

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## গভীর ডাইভ

Go এর `encoding/json` প্যাকেজ একটি সরলভাবে API প্রদান করে যা JSON ম্যানিপুলেশনের জটিলতা অনেকাংশে অ্যাব্সট্রাক্ট করে। Go এর বিকাশের শুরুতে প্রবর্তিত, এই প্যাকেজটি Go এর সাদাসিধে ও কার্যক্ষমতার দর্শনকে প্রতিফলিত করে। তবে, `encoding/json` এর রানটাইমে স্ট্রাক্টগুলি পরীক্ষা করার এবং পরিবর্তনের জন্য রিফ্লেকশনের ব্যবহার সিপিউ-ইনটেনসিভ পরিস্থিতিতে অ-আদর্শ পারফরমেন্সের কারণ হতে পারে।

`json-iterator/go` এবং `ffjson` এর মতো বিকল্পগুলো উঠে এসেছে, যা স্ট্যাটিক মার্শালিং এবং আনমার্শালিং কোড জেনারেট করে দ্রুত JSON প্রক্রিয়াকরণ প্রদান করে। তবে, এর সাদাসিধে, দৃঢ়তা, এবং ই স্ট্যান্ডার্ড লাইব্রেরির অংশ হিসেবে সব চেয়ে বেশি ব্যবহারিত `encoding/json` প্যাকেজ থাকে, যা Go সংস্করণগুলি জুড়ে সামঞ্জস্যতা এবং স্থায়িত্ব নিশ্চিত করে।

এর আপেক্ষিকভাবে ধীর পারফরমেন্স সত্ত্বেও, ব্যবহারে সহজতা এবং Go এর টাইপ সিস্টেমের সাথে একীভূততা `encoding/json` কে বেশিরভাগ অ্যাপ্লিকেশনের জন্য উপযুক্ত করে তোলে। যারা পারফরমেন্স সর্বোচ্চ গুরুত্ব প্রাপ্ত পরিবেশে কাজ করছেন, তাদের জন্য বাহ্যিক লাইব্রেরিগুলি অন্বেষণ করা মূল্যবান হতে পারে, কিন্তু অনেকের জন্য, স্ট্যান্ডার্ড লাইব্রেরি গতি, সাদাসিধে এবং বিশ্বাসযোগ্যতার মধ্যে সঠিক ভারসাম্য স্থাপন করে।
