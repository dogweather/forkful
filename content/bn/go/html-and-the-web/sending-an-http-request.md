---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:18:16.236648-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Go \u09A4\u09C7, HTTP \u0985\u09A8\
  \u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB \u098F\u09AC\u0982\
  \ \u09B8\u09BE\u09A1\u09BC\u09BE \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\
  \u09BC\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 `net/http` \u09AA\u09CD\
  \u09AF\u09BE\u0995\u09C7\u099C \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A7\
  \u09BE\u09AA\u09C7 \u09A7\u09BE\u09AA\u09C7 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2 \u0995\u09C0\u09AD\u09BE\u09AC\
  \u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u2026"
lastmod: '2024-03-17T18:47:43.471404-06:00'
model: gpt-4-0125-preview
summary: "Go \u09A4\u09C7, HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\
  \u09BE\u09A8\u09CB \u098F\u09AC\u0982 \u09B8\u09BE\u09A1\u09BC\u09BE \u09AA\u09CD\
  \u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\
  \u09A8\u09C7 `net/http` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u0964 \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09A7\u09BE\u09AA\u09C7 \u09A7\u09BE\u09AA\u09C7 \u0989\
  \u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\
  \ \u0995\u09C0\u09AD\u09BE\u09AC\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\
  \u09BE\u09B0\u09A3 GET \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\
  \u09A8\u09CB \u09AF\u09BE\u09AF\u09BC \u0993 \u09B8\u09BE\u09A1\u09BC\u09BE \u09AA\
  \u09A1\u09BC\u09BE \u09AF\u09BE\u09AF\u09BC."
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

## কিভাবে:
Go তে, HTTP অনুরোধ পাঠানো এবং সাড়া প্রক্রিয়া করা মানে `net/http` প্যাকেজ ব্যবহার করা। এখানে একটি ধাপে ধাপে উদাহরণ দেওয়া হল কীভাবে একটি সাধারণ GET অনুরোধ পাঠানো যায় ও সাড়া পড়া যায়:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    // সম্পদের URL নির্ধারণ করুন
    url := "http://example.com"

    // GET অনুরোধ পাঠাতে http.Get ব্যবহার করুন
    resp, err := http.Get(url)
    if err != nil {
        log.Fatal(err)
    }
    // ফাংশনের শেষে সাড়া দেহ বন্ধ করুন
    defer resp.Body.Close()

    // সাড়া দেহ পড়ুন
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // সাড়া দেহকে একটি স্ট্রিংয়ে রূপান্তর করুন এবং এটি প্রিন্ট করুন
    fmt.Println(string(body))
}
```

নমুনা আউটপুট (সংক্ষিপ্ততার জন্য ছোট):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

ফর্ম ডেটা সহ POST অনুরোধ পাঠাতে, আপনি `http.PostForm` ব্যবহার করতে পারেন:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
    "net/url"
)

func main() {
    // URL এবং ফর্ম ডেটা নির্ধারণ করুন
    url := "http://example.com/form"
    data := url.Values{}
    data.Set("key", "value")

    // ফর্ম ডেটা সহ POST অনুরোধ পাঠান
    resp, err := http.PostForm(url, data)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // সাড়া পড়ুন এবং প্রিন্ট করুন
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }

    fmt.Println(string(body))
}
```

## গভীর ডুব
Go-এর `net/http` প্যাকেজ HTTP সার্ভারের সাথে মিথস্ক্রিয়া করার জন্য একটি শক্তিশালী ও নমনীয় উপায় প্রদান করে। এর ডিজাইন Go-এর সারল্য, দক্ষতা এবং দৃঢ়তার উপর জোর দেয়। মূলত, JSON বা XML পেলোড সম্পাদনা জন্য ম্যানুয়ালি অনুরোধ দেহ তৈরি করা এবং উপযুক্ত শিরোনাম সেট করা প্রয়োজন ছিল। যেমন Go উন্নত হয়েছে, সম্প্রদায় রুটিং জন্য `gorilla/mux` এবং JSON পরিচালনা জন্য `gjson` এর মতো উচ্চস্তরের প্যাকেজ উন্নত করেছে।

Go-এর HTTP ক্লায়েন্টের একটি উল্লেখযোগ্য দিক হল এর ইন্টারফেস এবং স্ট্রাক্টর্্স, যেমন `http.Client` এবং `http.Request` ব্যবহার করে বিস্তারিত রূপান্তর এবং পরীক্ষা করাকে অনুমোদন করে। উদাহরণস্বরূপ, আপনি `http.Client` কে অনুরোধের সময়সীমা বা কর্মক্ষমতা বৃদ্ধি করার জন্য সংযোগ জীবিত রাখতে পরিবর্তন করতে পারেন।

সহজ HTTP মিথস্ক্রিয়ার জন্য একটি বিবেচিত বিকল্প "Resty" বা "Gentleman" এর মতো তৃতীয়-পক্ষের লাইব্রেরিগুলি ব্যবহার করা। এই প্যাকেজগুলি HTTP অনুরোধগুলির জন্য আরও উচ্চস্তরের ধারণা প্রদান করে, সাধারণ কাজগুলিকে আরও সংক্ষিপ্ত করে। তবে, আরও জটিল বা অনন্য HTTP মিথস্ক্রিয়ার দৃশ্যকল্প সমাধান করতে `net/http` প্যাকেজের বোঝা এবং ব্যবহার করা অপরিহার্য, যা Go-এর সমান্তরাল বৈশিষ্ট্য এবং শক্তিশালী মানক লাইব্রেরি সম্পূর্ণ ব্যবহার করা প্রতিষ্ঠান প্রদান করে।
