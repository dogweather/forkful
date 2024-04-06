---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:40:26.391136-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Go \u09A4\u09C7 \u099F\u09C7\u0995\
  \u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\u09C7\u0996\u09BE \u09B9\
  \u09DF `os` \u098F\u09AC\u0982 `io/ioutil` (Go \u09B8\u0982\u09B8\u09CD\u0995\u09B0\
  \u09A3 <1.16 \u098F\u09B0 \u099C\u09A8\u09CD\u09AF) \u0985\u09A5\u09AC\u09BE `os`\
  \ \u098F\u09AC\u0982 `io` \u09AA\u09CD\u09B2\u09BE\u09B8 `os` \u09AA\u09CD\u09AF\
  \u09BE\u0995\u09C7\u099C\u0997\u09C1\u09B2\u09BF\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 (Go 1.16 \u098F\u09AC\u0982 \u09A4\u09BE\u09B0\u2026"
lastmod: '2024-04-05T21:53:51.449825-06:00'
model: gpt-4-0125-preview
summary: "Go \u09A4\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2\
  \u09C7 \u09B2\u09C7\u0996\u09BE \u09B9\u09DF `os` \u098F\u09AC\u0982 `io/ioutil`\
  \ (Go \u09B8\u0982\u09B8\u09CD\u0995\u09B0\u09A3 <1.16 \u098F\u09B0 \u099C\u09A8\
  \u09CD\u09AF) \u0985\u09A5\u09AC\u09BE `os` \u098F\u09AC\u0982 `io` \u09AA\u09CD\
  \u09B2\u09BE\u09B8 `os` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u0997\u09C1\u09B2\
  \u09BF\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 (Go 1.16 \u098F\u09AC\u0982\
  \ \u09A4\u09BE\u09B0 \u098A\u09B0\u09CD\u09A7\u09CD\u09AC\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF) \u09AF\u09BE Go \u098F\u09B0 \u09B8\u09BE\u09A6\u09BE\u09B8\u09BF\u09A7\
  \u09BE \u0993 \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09BE\u09B0\u09BF\u09A4\u09BE\
  \u09B0 \u09A6\u09B0\u09CD\u09B6\u09A8\u0995\u09C7 \u09AA\u09CD\u09B0\u09A4\u09BF\
  \u09AB\u09B2\u09BF\u09A4 \u0995\u09B0\u09C7\u0964 \u09A8\u09A4\u09C1\u09A8 API \u09B8\
  \u09BE\u09A6\u09BE\u09B8\u09BF\u09A7\u09BE \u09A4\u09CD\u09B0\u09C1\u099F\u09BF\
  \ \u09B9\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09B2\u09BF\u0982 \u09AF\u09C1\u0995\
  \u09CD\u09A4 \u0989\u09A8\u09CD\u09A8\u09A4 \u09AA\u09CD\u09B0\u09CD\u09AF\u09BE\
  \u0995\u099F\u09BF\u09B8 \u09AA\u09CD\u09B0\u099A\u09BE\u09B0 \u0995\u09B0\u09C7\
  \u0964 \u099A\u09B2\u09C1\u09A8 \u09A6\u09C7\u0996\u09BF, Go \u098F\u09B0 `os` \u09AA\
  \u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0993 \u09B2\u09C7\u0996\u09BE\
  \u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09DF\u09BE\u099F\u09BF \u0995\
  \u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09A4\u09C7 \u09B9\u09DF\u0964 \u09AA\
  \u09CD\u09B0\u09A5\u09AE\u09C7, \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\
  \u09B0\u09C1\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 Go \u09AA\u09B0\u09BF\u09AC\u09C7\
  \u09B6 \u09B8\u09C7\u099F \u0986\u09AA \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09AA\
  \u09CD\u09B0\u09B8\u09CD\u09A4\u09C1\u09A4\u0964 \u09A4\u09BE\u09B0\u09AA\u09B0\
  , \u098F\u0995\u099F\u09BF `.go` \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09C1\u09A8, \u09AF\u09C7\u09AE\u09A8, `writeText.go`, \u098F\u09AC\
  \u0982 \u098F\u099F\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 \u099F\u09C7\u0995\u09CD\
  \u09B8\u099F \u098F\u09A1\u09BF\u099F\u09B0 \u0985\u09A5\u09AC\u09BE IDE \u09A4\u09C7\
  \ \u0993\u09AA\u09C7\u09A8 \u0995\u09B0\u09C1\u09A8\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u0989\u09A6\u09BE\u09B9\u09B0\
  \u09A3 \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\u09B2, \u09AF\u09BE `example.txt` \u09A8\
  \u09BE\u09AE\u0995 \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0987\u09B2\u09C7 \u098F\
  \u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09B2\u09C7\u0996\
  \u09C7."
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কিভাবে:
Go তে টেক্সট ফাইলে লেখা হয় `os` এবং `io/ioutil` (Go সংস্করণ <1.16 এর জন্য) অথবা `os` এবং `io` প্লাস `os` প্যাকেজগুলির মাধ্যমে (Go 1.16 এবং তার ঊর্ধ্বের জন্য) যা Go এর সাদাসিধা ও কার্যকারিতার দর্শনকে প্রতিফলিত করে। নতুন API সাদাসিধা ত্রুটি হ্যান্ডলিং যুক্ত উন্নত প্র্যাকটিস প্রচার করে। চলুন দেখি, Go এর `os` প্যাকেজ ব্যবহার করে একটি টেক্সট ফাইল তৈরি ও লেখার প্রক্রিয়াটি কিভাবে করতে হয়।

প্রথমে, নিশ্চিত করুন আপনার Go পরিবেশ সেট আপ করা এবং প্রস্তুত। তারপর, একটি `.go` ফাইল তৈরি করুন, যেমন, `writeText.go`, এবং এটি আপনার টেক্সট এডিটর অথবা IDE তে ওপেন করুন।

এখানে একটি সহজ উদাহরণ দেওয়া হল, যা `example.txt` নামক একটি ফাইলে একটি স্ট্রিং লেখে:

```go
package main

import (
    "os"
    "log"
)

func main() {
    content := []byte("Hello, Wired readers!\n")

    // Create or overwrite the file example.txt
    err := os.WriteFile("example.txt", content, 0644)
    if err != nil {
        log.Fatal(err)
    }
}

```

আপনি যখন `go run writeText.go` দিয়ে এই কোড রান করবেন, এটি "Hello, Wired readers!" সম্বলিত একটি ফাইল `example.txt` তৈরি করবে (অথবা যদি এটি আগে থেকেই থাকে তবে তা ওভাররাইট করবে)।

### একটি ফাইলে যোগ করা
আপনি যদি কনটেন্ট যোগ করতে চান তবে কি হবে? Go এই প্রক্রিয়া সামলাতে একটি নমনীয় উপায় প্রদান করে:

```go
file, err := os.OpenFile("example.txt", os.O_APPEND|os.O_WRONLY|os.O_CREATE, 0644)
if err != nil {
    log.Fatal(err)
}
defer file.Close()

if _, err := file.WriteString("Appending more text.\n"); err != nil {
    log.Fatal(err)
}
```

এই স্নিপেটটি `example.txt` কে অ্যাপেন্ড মোডে ওপেন করে, একটি অতিরিক্ত লাইন লেখে, এবং ত্রুটি দেখা দিলেও ফাইলটি ঠিকমত ক্লোজ হওয়ার নিশ্চয়তা দেয়।

## গভীর ডুব
Go এর ফাইল হ্যান্ডলিং এপ্রোচের অগ্রগতি তার কোড সাদাসিধা ও কার্যকারিতার প্রতি প্রতিশ্রুতির প্রতিফলন। প্রাথমিক সংস্করণগুলিতে `ioutil` প্যাকেজের উপর আরও বেশি নির্ভরশীল ছিল, যা একটু বেশি বাচ্যাতীত হওয়ার এবং ত্রুটির সম্ভাবনাও একটু বেশি ছিল। সংস্করণ 1.16 থেকে `os` এবং `io` প্যাকেজগুলির কার্যকারিতা বাড়ানোর প্রতি ঝোঁক দেখায় Go এর ফাইল অপারেশনে মসৃণতা আনার প্রতি সচেষ্ট পদক্ষেপ।

যদিও Go এর বিল্ট-ইন লাইব্রেরি অনেক কেসেই যথেষ্ট, জটিল ফাইল অপারেশন বা বৃহত্তর ফ্রেমওয়ার্কের মধ্যে কাজ করার সময় বিকল্প প্যাকেজ বা বাহ্যিক লাইব্রেরিগুলি পছন্দ করা হতে পারে। তবে, সরাসরি, সহজ ফাইল লেখার কাজের জন্য, স্ট্যান্ডার্ড লাইব্রেরি প্রায়ই Go প্রোগ্রামিংয়ে সবচেয়ে কার্যকারিতামূলক ও আদর্শপথ প্রদান করে। ফাইল অপারেশনের জন্য সরল, আরও সংহত APIs এর দিকে প্রবাহ শুধুমাত্র Go কোড লেখা ও রক্ষণাবেক্ষণ সহজ করে না, বরং সাদাসিধা, পাঠযোগ্যতা এবং ব্যবহারিকতার ভাষার দর্শনকেও উত্সাহিত করে।
