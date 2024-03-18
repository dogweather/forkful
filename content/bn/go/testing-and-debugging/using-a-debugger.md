---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:04.980168-06:00
description: "\u0997\u09CB \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\
  \u0982\u09AF\u09BC\u09C7 \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5\
  \ \u09B9\u09B2\u09CB \u09B0\u09BE\u09A8\u09BF\u0982 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u0986\u099A\u09B0\u09A3 \u09AC\u09C1\u099D\
  \u09A4\u09C7 \u0985\u09A5\u09AC\u09BE \u09B8\u09AE\u09B8\u09CD\u09AF\u09BE \u09A8\
  \u09BF\u09B0\u09CD\u09A3\u09AF\u09BC\u09C7 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09C7\u09B0 \u0985\u09AC\u09B8\u09CD\u09A5\u09BE \u09AA\u09B0\u09BF\
  \u09A6\u09B0\u09CD\u09B6\u09A8 \u0993 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\
  \u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u099F\u09C2\u09B2\u09B8 \u09AC\u09BE\
  \u2026"
lastmod: '2024-03-17T18:47:43.480743-06:00'
model: gpt-4-0125-preview
summary: "\u0997\u09CB \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\
  \u0982\u09AF\u09BC\u09C7 \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5\
  \ \u09B9\u09B2\u09CB \u09B0\u09BE\u09A8\u09BF\u0982 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u0986\u099A\u09B0\u09A3 \u09AC\u09C1\u099D\
  \u09A4\u09C7 \u0985\u09A5\u09AC\u09BE \u09B8\u09AE\u09B8\u09CD\u09AF\u09BE \u09A8\
  \u09BF\u09B0\u09CD\u09A3\u09AF\u09BC\u09C7 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09C7\u09B0 \u0985\u09AC\u09B8\u09CD\u09A5\u09BE \u09AA\u09B0\u09BF\
  \u09A6\u09B0\u09CD\u09B6\u09A8 \u0993 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\
  \u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u099F\u09C2\u09B2\u09B8 \u09AC\u09BE\
  \u2026"
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?

গো প্রোগ্রামিংয়ে ডিবাগার ব্যবহার করার অর্থ হলো রানিং প্রোগ্রামের আচরণ বুঝতে অথবা সমস্যা নির্ণয়ে প্রোগ্রামের অবস্থা পরিদর্শন ও পরিবর্তনের জন্য টূলস বা বৈশিষ্ট্য ব্যবহার। প্রোগ্রামাররা তাদের কোডের শুদ্ধতা নিশ্চিত করতে, পারফরম্যান্স অপ্টিমাইজ করতে এবং বাগ দ্রুত খুঁজে বের করে ফিক্স করার লক্ষ্যে এটি করে থাকেন।

## কিভাবে:

গো `delve` নামে একটি বিল্ট-ইন সুবিধা ডিবাগিংয়ের জন্য প্রদান করে। এটি একটি পূর্ণাঙ্গ ফিচারযুক্ত ডিবাগিং টূল যা আপনাকে গো প্রোগ্রাম ধাপে ধাপে এক্সিকিউট করতে, প্রোগ্রাম ভ্যারিয়েবল পরিদর্শন এবং প্রকাশ মূল্যায়ন করতে দেয়।

শুরু করতে, আপনাকে প্রথমে `delve` ইনস্টল করতে হবে। এটি নিম্নলিখিত চালানোর মাধ্যমে করা যায়:

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

এখন, আসুন একটি সাধারণ গো প্রোগ্রাম ডিবাগ করা যাক। `main.go` প্রোগ্রামটি বিবেচনা করুন:

```go
package main

import "fmt"

func main() {
    message := "Debugging in Go"
    fmt.Println(message)
}
```

এই প্রোগ্রামটি ডিবাগ শুরু করতে, প্রজেক্টের ডিরেক্টরিতে একটি টার্মিনাল খুলুন এবং নিম্নলিখিত চালান:

```shell
dlv debug
```

এই কমান্ডটি অপ্টিমাইজেশন নিষ্ক্রিয় করে (ডিবাগিং অভিজ্ঞতা উন্নত করার জন্য) প্রোগ্রামটি কম্পাইল করে, এটি শুরু করে এবং একটি ডিবাগার এটাতে যুক্ত করে।

`delve` চালু হয়ে গেলে, আপনি ইন্টার‍্যাকটিভ ডিবাগার শেলে আছেন। এখানে কয়েকটি মৌলিক কমান্ড হল:

- `break main.main` মেইন ফাংশনে একটি ব্রেকপয়েন্ট সেট করে।
- `continue` ব্রেকপয়েন্ট পাওয়া পর্যন্ত প্রোগ্রামের এক্সিকিউশন অব্যাহত রাখে।
- `print message` `message` ভ্যারিয়েবলের মান প্রিন্ট করবে।
- `next` প্রোগ্রাম এক্সিকিউশনটি পরবর্তী লাইনে এগিয়ে নেয়।
- `quit` ডিবাগার থেকে বের হয়ে যায়।

ব্রেকপয়েন্টে পৌঁছে এবং ভ্যারিয়েবল প্রিন্ট করার সময় আউটপুট এরকম দেখাবে:

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (hits goroutine(1):1 total:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Debugging in Go"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Debugging in Go"
```

এই কমান্ডগুলো ব্যবহার করে, আপনি আপনার প্রোগ্রাম ধাপে ধাপে এগিয়ে নিতে পারবেন, অবস্থান পরিদর্শন করতে পারবেন যাতে এটি কিভাবে আচরণ করছে তা বুঝতে পারেন, এবং কোন সমস্যা চিহ্নিত করতে পারেন।

## গভীর ডুব

যেহেতু গো'র নির্বাহন মডেল এবং রানটাইমের প্রকৃতি বিবেচনায় গো ডেভেলপারদের জন্য `delve` কে জিডিবি (জিএনইউ ডিবাগার) এর তুলনায় ঐতিহ্যগত টূলস হিসেবে গো'র ডিবাগিং টূল হিসেবে বেছে নেওয়া হয়েছে। জিডিবি প্রাথমিকভাবে গো রানটাইমের সাথে মাথায় রেখে ডিজাইন করা হয়নি, ফলে `delve` গো ডেভেলপারদের জন্য আরও উপযুক্ত পছন্দ হয়ে ওঠে। `Delve` বিশেষ করে গো এর জন্য ডিজাইন করা হয়েছে, গো রুটিন, চ্যানেলস, এবং অন্যান্য গো-নির্দিষ্ট কনস্ট্রাক্টসের জন্য আরও সহজাত ডিবাগিং অভিজ্ঞতা প্রদান করে।

পাশাপাশি, `delve` গো প্রোগ্রাম নিয়ে কাজ করার সময় জিডিবি দ্বারা প্রদত্ত মৌলিক বৈশিষ্ট্যগুলির চেয়ে এর বিস্তৃত ফিচার সমর্থন করে। এর মধ্যে অন্তর্ভুক্ত কিন্তু সীমাবদ্ধ নয়: ডিবাগিংয়ের জন্য চলমান প্রক্রিয়াগুলিতে সংযুক্ত হওয়া; শর্তাধীন ব্রেকপয়েন্ট; এবং গো'র সমান্তরালতা প্রিমিটিভগুলি জড়িত জটিল প্রকাশ মূল্যায়ন।

`delve` অনেক গো ডেভেলপারের জন্য যাওয়ার ডিবাগার হলেও, লক্ষণীয় যে গো টুলচেইনও প্রোফাইলিংয়ের জন্য বিল্ট-ইন `pprof` টুল এবং সমান্তরালতা ভিজ্যুয়ালাইজেশনের জন্য `trace` টুলের মতো হালকা-ওজন ডিবাগিং সাপোর্ট অন্তর্ভুক্ত করে। এই টুলগুলি মাঝে মাঝে প্রোগ্রাম পারফরম্যান্স সমস্যা বা সমান্তরালতা বাগ নির্ণয়ের জন্য দ্রুত বা আরও উচ্চ-স্তরের পথ প্রদান করতে পারে, যা ডিবাগিং প্রসঙ্গে পরিপূরক বা এমনকি পছন্দনীয় হতে পারে।