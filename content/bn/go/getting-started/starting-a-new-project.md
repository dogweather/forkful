---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:20:49.694076-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: \u09AA\u09CD\u09B0\u09A5\u09AE\
  \u09C7, \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09C1\u09A8 \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09BE\
  \ \u0986\u099B\u09C7 \u0995\u09BF \u09A8\u09BE Go, \u0986\u09AA\u09A8\u09BE\u09B0\
  \ \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\u09B2\u09C7 `go version` \u099A\
  \u09BE\u09B2\u09BE\u09A8\u0964 \u0986\u09AA\u09A8\u09BF \u0987\u09A8\u09B8\u09CD\
  \u099F\u09B2 \u0995\u09B0\u09BE Go \u098F\u09B0 \u09B8\u0982\u09B8\u09CD\u0995\u09B0\
  \u09A3 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09B9\u09BF\u09B8\u09BE\u09AC\u09C7\
  \ \u09A6\u09C7\u0996\u09A4\u09C7 \u09AA\u09BE\u09AC\u09C7\u09A8\u0964\u2026"
lastmod: '2024-03-17T18:47:43.476277-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\
  \u09A4 \u0995\u09B0\u09C1\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 \u0987\u09A8\u09B8\
  \u09CD\u099F\u09B2 \u0995\u09B0\u09BE \u0986\u099B\u09C7 \u0995\u09BF \u09A8\u09BE\
  \ Go, \u0986\u09AA\u09A8\u09BE\u09B0 \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\
  \u09B2\u09C7 `go version` \u099A\u09BE\u09B2\u09BE\u09A8\u0964 \u0986\u09AA\u09A8\
  \u09BF \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09BE Go \u098F\u09B0 \u09B8\
  \u0982\u09B8\u09CD\u0995\u09B0\u09A3 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09B9\
  \u09BF\u09B8\u09BE\u09AC\u09C7 \u09A6\u09C7\u0996\u09A4\u09C7 \u09AA\u09BE\u09AC\
  \u09C7\u09A8\u0964 \u09AA\u09B0\u09C7\u09B0 \u09A7\u09BE\u09AA\u09C7, \u0986\u09B8\
  \u09C1\u09A8 \u098F\u0995\u099F\u09BF \u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\
  \u099C\u09C7\u0995\u09CD\u099F \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BF\u0964\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09CD\u09B7\
  \u09C7\u09A4\u09CD\u09B0\u09C7 \u09AF\u09BE\u09A8 \u098F\u09AC\u0982 \u099A\u09BE\
  \u09B2\u09BE\u09A8."
title: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BE"
weight: 1
---

## কীভাবে:
প্রথমে, নিশ্চিত করুন আপনার ইনস্টল করা আছে কি না Go, আপনার টার্মিনালে `go version` চালান। আপনি ইনস্টল করা Go এর সংস্করণ আউটপুট হিসাবে দেখতে পাবেন। পরের ধাপে, আসুন একটি নতুন প্রজেক্ট শুরু করি। আপনার কার্যক্ষেত্রে যান এবং চালান:

```shell
mkdir hello-world
cd hello-world
```

এটি আপনার প্রজেক্টের জন্য একটি নতুন ডিরেক্টরি তৈরি করে এবং আপনাকে সেখানে নিয়ে যায়। এখন, মডিউলটি সংগঠিত করুন:

```shell
go mod init example.com/hello-world
```

`example.com/hello-world` কে আপনার মডিউল পথের সাথে প্রতিস্থাপন করুন। এই কমান্ডটি আপনার ডিরেক্টরিতে একটি `go.mod` ফাইল তৈরি করে, যা নতুন একটি Go মডিউল আরম্ভের সংকেত দেয়। এই রকম দেখতে পারে `go.mod` :

```plaintext
module example.com/hello-world

go 1.18
```

`go.mod` আপনার প্রজেক্টের নির্ভরতা ট্র্যাক করে। এখন, একটি `main.go` ফাইল তৈরি করুন:

```shell
touch main.go
```

`main.go` ফাইলটি আপনার পছন্দের এডিটরে খুলুন এবং "Hello, World!" প্রিন্ট করার জন্য নিম্নোক্ত কোড যোগ করুন:

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
```

আপনার প্রোগ্রাম চালানোর জন্য, টার্মিনালে ফিরে যান এবং নিম্নলিখিত কমান্ড চালান:

```shell
go run main.go
```

আপনি দেখতে পাবেন:

```plaintext
Hello, World!
```

অভিনন্দন! আপনি সবে একটি নতুন Go প্রজেক্ট শুরু করেছেন এবং আপনার প্রথম Go প্রোগ্রাম চালিয়েছেন।

## গভীর ডুব
Go তে নির্ভরতা ব্যবস্থাপনার মানদণ্ড হিসেবে মডিউলকে প্রবর্তনের উদ্যোগটি ছিল Go ইকোসিস্টেমে এক গুরুত্বপূর্ণ পরিবর্তন, যা আনুষ্ঠানিকভাবে Go 1.11 এ গৃহীত হয়েছিল। মডিউলের আগে, Go ডেভেলপাররা GOPATH পরিবেশ ভেরিয়েবলের উপর নির্ভর করে নির্ভরতা পরিচালনা করত, যা কম সহজবোধ্য ছিল এবং প্রায়শই "নির্ভরতা নরকে" পরিণত হত।

মডিউল প্রকল্পের নির্ভরতা, সংস্করণ পরিচালনা করার অন্তর্বুদ্ধ উপায় প্রদান করে এবং Go প্রকল্পগুলিকে আরও স্বাধীন এবং বহনযোগ্য করার দিকে অগ্রসর করে। প্রতিটি মডিউল এর নির্ভরতা উল্লেখ করে যা Go `go.mod` ফাইলে ট্র্�
