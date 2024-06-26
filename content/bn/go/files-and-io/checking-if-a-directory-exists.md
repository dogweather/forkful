---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:52.694304-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Go \u09AD\u09BE\u09B7\u09BE\u09AF\
  \u09BC, `os` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u099F\u09BF \u0985\u09AA\
  \u09BE\u09B0\u09C7\u099F\u09BF\u0982 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AE\u09BF\u09A5\u09B8\u09CD\u0995\u09CD\u09B0\
  \u09BF\u09AF\u09BC\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AB\u09BE\u0982\u09B6\
  \u09A8\u09BE\u09B2\u09BF\u099F\u09BF\u09B8 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8\
  \ \u0995\u09B0\u09C7, \u09AF\u09BE \u098F\u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\
  \u09A6\u09C7\u09B6\u09BF\u0995\u09BE \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC\
  \ \u0986\u099B\u09C7 \u0995\u09BF\u09A8\u09BE \u09A4\u09BE \u09AF\u09BE\u099A\u09BE\
  \u0987 \u0995\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.492094-06:00'
model: gpt-4-0125-preview
summary: "Go \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC, `os` \u09AA\u09CD\u09AF\u09BE\u0995\
  \u09C7\u099C\u099F\u09BF \u0985\u09AA\u09BE\u09B0\u09C7\u099F\u09BF\u0982 \u09B8\
  \u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AE\
  \u09BF\u09A5\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09AB\u09BE\u0982\u09B6\u09A8\u09BE\u09B2\u09BF\u099F\u09BF\u09B8\
  \ \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, \u09AF\u09BE \u098F\u0995\
  \u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6\u09BF\u0995\u09BE \u0985\
  \u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u0986\u099B\u09C7 \u0995\u09BF\u09A8\
  \u09BE \u09A4\u09BE \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE \u09B8\u09AE\
  \u09CD\u09AD\u09AC \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0986\
  \u09AA\u09A8\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u09A4\u09BE \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কিভাবে:
Go ভাষায়, `os` প্যাকেজটি অপারেটিং সিস্টেমের সাথে মিথস্ক্রিয়ার জন্য ফাংশনালিটিস প্রদান করে, যা একটি নির্দেশিকা অস্তিত্ব আছে কিনা তা যাচাই করা সম্ভব করে। এখানে আপনি কিভাবে তা করতে পারেন:

```go
package main

import (
    "fmt"
    "os"
)

// isDirExists যাচাই করে যে একটি নির্দেশিকা অস্তিত্ব আছে কি না
func isDirExists(path string) bool {
    info, err := os.Stat(path)
    if os.IsNotExist(err) {
        return false
    }
    return info.IsDir()
}

func main() {
    dirPath := "/tmp/exampleDir"

    if isDirExists(dirPath) {
        fmt.Printf("নির্দেশিকা %s অস্তিত্ব আছে.\n", dirPath)
    } else {
        fmt.Printf("নির্দেশিকা %s অস্তিত্ব নেই.\n", dirPath)
    }
}
```
উদাহরণ আউটপুট:

```
নির্দেশিকা /tmp/exampleDir অস্তিত্ব আছে.
```
অথবা 

```
নির্দেশিকা /tmp/exampleDir অস্তিত্ব নেই.
```

নির্ভর করে `/tmp/exampleDir` অস্তিত্ব আছে কিনা।

## গভীর ডাইভ
ফাংশন `os.Stat` একটি `FileInfo` ইন্টারফেস এবং একটি ত্রুটি ফেরত দেয়। যদি ত্রুটিটি `os.ErrNotExist` ধরণের হয়, তার মানে নির্দেশিকাটি অস্তিত্ব নেই। যদি কোনো ত্রুটি না থাকে, তাহলে আমরা আরও যাচাই করি যে পথটি সত্যিই একটি নির্দেশিকাকে বোঝায় কিনা `FileInfo` ইন্টারফেস থেকে `IsDir()` মেথডের মাধ্যমে।

এই পদ্ধতিটি এর সরলতা এবং কার্যকারিতার জন্য বৈশিষ্ট্যমণ্ডিত, তবে এটি উল্লেখ করা জরুরি যে তৈরি বা লিখনের মতো অপারেশন করার আগে নির্দেশিকার অস্তিত্ব যাচাই করা সমান্তরাল পরিবেশে রেস কন্ডিশনের সৃষ্টি করতে পারে। অনেক পরিস্থিতিতে, বিশেষ করে সমান্তরাল অ্যাপ্লিকেশনগুলিতে, অপারেশনটি (যেমন, ফাইল তৈরি) চেষ্টা করা এবং তারপর ত্রুটি সম্পর্কে সমাধান করা, প্রথমে যাচাই করার চেয়ে নিরাপদ হতে পারে।

ইতিহাসগতভাবে, এই পদ্ধতিটি এর সরল যুক্তিবিদ্যার কারণে প্রোগ্রামিংয়ে সাধারণ ছিল। যাইহোক, মাল্টি-থ্রেডেড এবং সমান্তরাল কম্পিউটিং এর বিকাশ আরও শক্তিশালী ত্রুটি হ্যান্ডলিং এবং এই ধরণের প্রাক-শর্ত যাচাই এড়িয়ে চলার দিকে যেতে বাধ্য করে। এটি তার উপযোগিতা হ্রাস করে না সিম্পল, সিঙ্গেল-থ্রেডেড অ্যাপ্লিকেশন বা স্ক্রিপ্টগুলির জন্য যেখানে এই শর্তগুলি কম উদ্বেগের কারণ।
