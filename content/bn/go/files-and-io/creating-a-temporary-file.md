---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:26.633804-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Go-\u09A4\u09C7, `ioutil` \u09AA\
  \u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AE\u09C2\u09B2\u09A4 \u0985\u09B8\u09CD\
  \u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u0987\u0989\u099F\u09BF\u09B2\u09BF\u099F\u09BF\
  \u09B8 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u099B\u09BF\u09B2\
  \u0964 \u09A4\u09AC\u09C7, Go 1.16 `os` \u098F\u09AC\u0982 `io/ioutil` \u09AA\u09CD\
  \u09AF\u09BE\u0995\u09C7\u099C\u09C7\u09B0 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\
  \u09B2\u09BF\u0995\u09C7 \u0986\u09B0\u0993\u2026"
lastmod: '2024-04-05T21:53:51.452686-06:00'
model: gpt-4-0125-preview
summary: "Go-\u09A4\u09C7, `ioutil` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AE\
  \u09C2\u09B2\u09A4 \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\
  \u0987\u09B2 \u09A4\u09C8\u09B0\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF \u0987\u0989\
  \u099F\u09BF\u09B2\u09BF\u099F\u09BF\u09B8 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8\
  \ \u0995\u09B0\u09C7\u099B\u09BF\u09B2\u0964 \u09A4\u09AC\u09C7, Go 1.16 `os` \u098F\
  \u09AC\u0982 `io/ioutil` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u09C7\u09B0\
  \ \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u0986\u09B0\
  \u0993 \u09B8\u09C1\u09AC\u09BF\u09A8\u09CD\u09AF\u09B8\u09CD\u09A4 \u09B8\u09CD\
  \u09A5\u09BE\u09A8\u09C7 \u09AA\u09CD\u09B0\u099A\u09BE\u09B0 \u0995\u09B0\u09C7\
  \u099B\u09C7\u0964 \u098F\u0996\u09A8, `os` \u098F\u09AC\u0982 `io` \u09AA\u09CD\
  \u09AF\u09BE\u0995\u09C7\u099C\u0997\u09C1\u09B2\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\
  \u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09B8\u09AE\u09CD\u09AA\u09BE\u09A6\
  \u09A8\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09BE\u09A7\u09BE\
  \u09A8\u09CD\u09AF \u09AA\u09BE\u09AF\u09BC\u0964 \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\
  \u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF, \u09B2\u09C7\u0996\u09BE \u098F\u09AC\
  \u0982 \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE\u09B0 \u09A7\u09BE\u09AA\
  \u09C7 \u09A7\u09BE\u09AA\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6\u09BF\
  \u0995\u09BE \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09BE \u09B9\u09B2\
  ."
title: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
weight: 21
---

## কিভাবে:
Go-তে, `ioutil` প্যাকেজ মূলত অস্থায়ী ফাইল তৈরির জন্য ইউটিলিটিস প্রদান করেছিল। তবে, Go 1.16 `os` এবং `io/ioutil` প্যাকেজের ফাংশনগুলিকে আরও সুবিন্যস্ত স্থানে প্রচার করেছে। এখন, `os` এবং `io` প্যাকেজগুলি অস্থায়ী ফাইল সম্পাদনার জন্য প্রাধান্য পায়।

এখানে একটি অস্থায়ী ফাইল তৈরি, লেখা এবং মুছে ফেলার ধাপে ধাপে নির্দেশিকা প্রদান করা হল:

১. **একটি অস্থায়ী ফাইল তৈরি:**

`os.CreateTemp` ফাংশন ব্যবহার করে, আপনি একটি অস্থায়ী ফাইল তৈরি করতে পারবেন। কোন ডিরেক্টরি নির্দিষ্ট না করে, এটি আপনার OS-এর ডিফল্ট টেম্প ফোল্ডার ব্যবহার করে।

```go
package main

import (
    "io/ioutil"
    "log"
    "os"
)

func main() {
    tmpFile, err := ioutil.TempFile("", "example.*.txt")
    if err != nil {
        log.Fatal(err)
    }
    log.Printf("Created temporary file: %s\n", tmpFile.Name())

    defer os.Remove(tmpFile.Name()) // পরিষ্কারের জন্য
}
```

২. **অস্থায়ী ফাইলে লেখা:**

ফাইলে লিখন কাজটি `Write` মেথড বা `io` বা `bufio` প্যাকেজ থেকে অন্যান্য লেখা ফাংশনগুলির মাধ্যমে অর্জন করা যেতে পারে।

```go
_, err = tmpFile.Write([]byte("Hello, World!"))
if err != nil {
    log.Fatal(err)
}
```

৩. **অস্থায়ী ফাইল থেকে পড়া:**

পড়ার কাজটি অনুরূপভাবে ফাইলের `Read` মেথড বা `io` বা `bufio` প্যাকেজ থেকে ইউটিলিটিগুলি ব্যবহার করে করা যায়।

```go
data, err := ioutil.ReadFile(tmpFile.Name())
if err != nil {
    log.Fatal(err)
}
log.Printf("Data read: %s\n", string(data))
```

৪. **অস্থায়ী ফাইলটি মুছে ফেলা:**

তৈরি করার পর্যায়ে `defer os.Remove(tmpFile.Name())` বিবৃতি প্রদান করে নিশ্চিত করে যে প্রোগ্রাম সমাপ্তির পর অস্থায়ী ফাইলটি মুছে ফেলা হয়, প্রয়োজন মতো স্পষ্টভাবে মুছে ফেলা পরিচালনা করা যেতে পারে।

নমুনা আউটপুট:
```
2023/04/01 15:00:00 Created temporary file: /tmp/example.123456.txt
2023/04/01 15:00:00 Data read: Hello, World!
```

## গভীর ডুব
Go-তে অস্থায়ী ফাইলগুলির সম্পাদনা পিছনের কারিগরি বিকশিত হয়েছে। প্রাথমিকভাবে, অস্থায়ী ফাইলগুলি তৈরি করা প্রধানত `ioutil.TempFile` ফাংশন দ্বারা পরিচালিত হতো, যা আরও নিরাপদ এবং কার্যকর ফাইল সম্পাদনা প্রক্রিয়ার দিকে সফটওয়্যার ডেভেলপমেন্টের প্রবণতা প্রতিফলিত করে। `os` এবং `io` প্যাকেজগুলিতে এই ক্রিয়াকলাপগুলির সংহত করার সঙ্গে Go 1.16-এর চলে আসা ভাষার মানক লাইব্রেরি প্রসারিত এবং ঐক্যবদ্ধ এবং একগুচ্ছ API ব্যবহারের প্রতি উত্সাহিত করার দিকে আরও বড় ধাক্কা দেয়। 

যদিও অস্থা�
