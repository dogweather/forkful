---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:04:39.767783-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Go \u09A4\u09C7 HTML \u09AA\u09BE\
  \u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u09B8\u09BE\
  \u09A7\u09BE\u09B0\u09A3\u09A4 `goquery` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\
  \ \u0985\u09A5\u09AC\u09BE \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\
  \u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0\
  \ `net/html` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3\u2026"
lastmod: '2024-03-17T18:47:43.472466-06:00'
model: gpt-4-0125-preview
summary: "Go \u09A4\u09C7 HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF, \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 `goquery`\
  \ \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u0985\u09A5\u09AC\u09BE \u09B8\u09CD\
  \u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 `net/html` \u09AA\u09CD\u09AF\u09BE\u0995\
  \u09C7\u099C \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE \u09B9\
  \u09AF\u09BC\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09AE\
  \u09CC\u09B2\u09BF\u0995 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\
  \u09AF\u09BC\u09BE \u09B9\u09B2 `net/html` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\
  \u09C7\u099C \u09A5\u09C7\u0995\u09C7 \u09B8\u09AC \u09B2\u09BF\u0999\u09CD\u0995\
  \ \u09A8\u09BF\u09B0\u09CD\u09AF\u09BE\u09B8 \u0995\u09B0\u09BE\u09B0."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কিভাবে:
Go তে HTML পার্স করার জন্য, সাধারণত `goquery` প্যাকেজ অথবা স্ট্যান্ডার্ড লাইব্রেরির `net/html` প্যাকেজ ব্যবহার করা হয়। এখানে একটি মৌলিক উদাহরণ দেওয়া হল `net/html` ব্যবহার করে একটি ওয়েবপেজ থেকে সব লিঙ্ক নির্যাস করার:

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // HTML ডকুমেন্ট পাওয়া
    res, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // HTML ডকুমেন্ট পার্স করা
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // DOM এর মধ্যে পুনরাবৃত্তিমূলক ভ্রমণ করার ফাংশন
    var f func(*html.Node)
    f = func(n *html.Node) {
        if n.Type == html.ElementNode and n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    fmt.Println(a.Val)
                    break
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            f(c)
        }
    }

    // DOM এর মধ্যে ভ্রমণ করা
    f(doc)
}
```

নমুনা আউটপুট (ধরা যাক `http://example.com` এ দুটি লিঙ্ক আছে):

```
http://www.iana.org/domains/example
http://www.iana.org/domains/reserved
```

এই কোড HTML পৃষ্ঠা অনুরোধ করে, এটি পার্স করে এবং পুনরাবৃত্তিমূলকভাবে DOM এর মধ্যে ভ্রমণ করে `<a>` ট্যাগের সকল `href` এট্রিবিউট খুঁজে বের করে এবং প্রিন্ট করে।

## গভীর ডুব
`net/html` প্যাকেজ Go তে HTML পার্সিংয়ের মূলভিত্তি প্রদান করে, HTML5 স্ট্যান্ডার্ড দ্বারা নির্দিষ্ট টোকেনাইজেশান এবং ট্রি নির্মাণ অ্যালগরিদমগুলিকে সরাসরি বাস্তবায়ন করে। এই নিম্নস্তরীয় পদ্ধতি শক্তিশালী কিন্তু জটিল কাজের জন্য বাচনবিলাসী হতে পারে।

অন্যদিকে, jQuery থেকে অনুপ্রাণিত তৃতীয় পক্ষের `goquery` প্যাকেজটি DOM ম্যানিপুলেশন এবং ট্র্যাভার্সাল সরলীকৃত করার একটি উচ্চস্তরের ইন্টারফেস প্রদান করে এবং এলিমেন্ট নির্বাচন, অ্যাট্রিবিউট এক্সট্রাকশান, এবং কনটেন্ট ম্যানিপুলেশানের মত কাজের জন্য সংক্ষিপ্ত এবং প্রকাশযোগ্য কোড লেখার সুবিধা দেয়।

তবে, `goquery` এর সুবিধার জন্য অতিরিক্ত নির্ভরতা এবং এই অ্যাবস্ট্র্যাকশান স্তরের ফলে সম্ভাব্য ধীর পারফরম্যান্স বলি দিতে হয়। `net/html` এবং `goquery` (অথবা অন্যান্য পার্সিং লাইব্রেরিগুলির) মধ্যে পছন্দের প্রশ্নটি প্রকল্পের নির্দিষ্ট প্রয়োজনীয়তার উপর নির্ভর করে, যেমন পারফরম্যান্স অপটিমাইজেশনের প্রয়োজন অথবা ব্যবহারের সহজতা।

ঐতিহাসিকভাবে, Go তে HTML পার্সিং মৌলিক স্ট্রিং অপারেশনগুলি থেকে সোফিস্টিকেটেড DOM ট্রি ম্যানিপুলেশানে বিবর্তিত হয়েছে, ভাষার বর্ধিত ইকোসিস্টেম এবং সমৃদ্ধ ওয়েব স্ক্র্যাপিং এবং ডেটা এক্সট্রাকশন টুলগুলির জন্য সম্প্রদায়ের চাহিদা প্রতিফলিত করে। স্থানীয় ক্ষমতাবলে সত্ত্বেও, `goquery` মতো তৃতীয়-পক্ষের লাইব্রেরিগুলির প্রচলন Go সম্প্রদায়ের মডিউলার, পুনঃব্যবহারযোগ্য কোডের পছন্দের পরিচয় বয়ে আনে। তবে পারফরম্যান্স-ক্রিটিক্যাল অ্যাপ্লিকেশনের জন্য, প্রোগ্রামাররা এখনও `net/html` প্যাকেজ পছন্দ করতে পারে অথবা সিম্পল পার্সিং কাজের জন্য regex ব্যবহার করতে পারে, regex-ভিত্তিক HTML পার্সিংয়ের অন্তর্নিহিত ঝুঁকি এবং সীমাবদ্ধতা মাথায় রেখে।
