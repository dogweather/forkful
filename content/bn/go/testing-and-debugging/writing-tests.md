---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:12.829174-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Go \u09AD\u09BE\u09B7\u09BE\u09AF\
  \u09BC, \u099F\u09C7\u09B8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09B8\u09BE\u09A7\
  \u09BE\u09B0\u09A3\u09A4 \u09AF\u09C7 \u0995\u09CB\u09A1\u09C7\u09B0 \u09B8\u09BE\
  \u09A5\u09C7 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09BE \u09B9\
  \u09AF\u09BC \u09B8\u09C7\u0987 \u098F\u0995\u0987 \u09AA\u09CD\u09AF\u09BE\u0995\
  \u09C7\u099C\u09C7 \u09B2\u09C7\u0996\u09BE \u09B9\u09AF\u09BC\u0964 \u099F\u09C7\
  \u09B8\u09CD\u099F \u09AF\u09C1\u0995\u09CD\u09A4 \u09AB\u09BE\u0987\u09B2\u0997\
  \u09C1\u09B2\u09BF `_test.go` \u09B8\u09BE\u09AB\u09BF\u0995\u09CD\u09B8 \u09A6\u09BF\
  \u09AF\u09BC\u09C7 \u09A8\u09BE\u09AE\u0995\u09B0\u09A3 \u0995\u09B0\u09BE\u2026"
lastmod: '2024-04-05T21:53:51.417247-06:00'
model: gpt-4-0125-preview
summary: "Go \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC, \u099F\u09C7\u09B8\u09CD\u099F\u0997\
  \u09C1\u09B2\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 \u09AF\u09C7 \u0995\
  \u09CB\u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AA\u09B0\u09C0\u0995\u09CD\
  \u09B7\u09BE \u0995\u09B0\u09BE \u09B9\u09AF\u09BC \u09B8\u09C7\u0987 \u098F\u0995\
  \u0987 \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u09C7 \u09B2\u09C7\u0996\u09BE\
  \ \u09B9\u09AF\u09BC\u0964 \u099F\u09C7\u09B8\u09CD\u099F \u09AF\u09C1\u0995\u09CD\
  \u09A4 \u09AB\u09BE\u0987\u09B2\u0997\u09C1\u09B2\u09BF `_test.go` \u09B8\u09BE\u09AB\
  \u09BF\u0995\u09CD\u09B8 \u09A6\u09BF\u09AF\u09BC\u09C7 \u09A8\u09BE\u09AE\u0995\
  \u09B0\u09A3 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\u0964 \u099F\u09C7\u09B8\u09CD\
  \u099F\u0997\u09C1\u09B2\u09BF \u098F\u09AE\u09A8 \u09AB\u09BE\u0982\u09B6\u09A8\
  \ \u09AF\u09BE `testing.T` \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u09C7\u09B0\
  \ \u098F\u0995\u099F\u09BF \u09AA\u09AF\u09BC\u09C7\u09A8\u09CD\u099F\u09BE\u09B0\
  \ \u09A8\u09C7\u09AF\u09BC (\u09AF\u09BE `testing` \u09AA\u09CD\u09AF\u09BE\u0995\
  \u09C7\u099C \u09A5\u09C7\u0995\u09C7 \u0986\u09B8\u09C7) \u098F\u09AC\u0982 `t.Fail()`,\
  \ `t.Errorf()`, \u0987\u09A4\u09CD\u09AF\u09BE\u09A6\u09BF \u09AE\u09C7\u09A5\u09A1\
  \ \u09A1\u09BE\u0995 \u0995\u09B0\u09C7 \u09AC\u09CD\u09AF\u09B0\u09CD\u09A5\u09A4\
  \u09BE \u09B8\u0982\u0995\u09C7\u09A4 \u0995\u09B0\u09C7\u0964 `math.go` \u09AE\u09A7\
  \u09CD\u09AF\u09C7 \u09B8\u0982\u099C\u09CD\u099E\u09BE\u09AF\u09BC\u09BF\u09A4\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u09AB\u09BE\u0982\
  \u09B6\u09A8 `Add` \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u099F\u09C7\u09B8\u09CD\
  \u099F\u09C7\u09B0 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কিভাবে:
Go ভাষায়, টেস্টগুলি সাধারণত যে কোডের সাথে পরীক্ষা করা হয় সেই একই প্যাকেজে লেখা হয়। টেস্ট যুক্ত ফাইলগুলি `_test.go` সাফিক্স দিয়ে নামকরণ করা হয়। টেস্টগুলি এমন ফাংশন যা `testing.T` অবজেক্টের একটি পয়েন্টার নেয় (যা `testing` প্যাকেজ থেকে আসে) এবং `t.Fail()`, `t.Errorf()`, ইত্যাদি মেথড ডাক করে ব্যর্থতা সংকেত করে।

`math.go` মধ্যে সংজ্ঞায়িত একটি সাধারণ ফাংশন `Add` এর জন্য টেস্টের উদাহরণ:
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

টেস্ট ফাইল `math_test.go`:
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; want %d", result, expected)
    }
}
```

আপনার টেস্টগুলি চালাতে `go test` কমান্ড আপনার টেস্ট ফাইলগুলির একই ডিরেক্টরিতে ব্যবহার করুন। পাস করা টেস্টের সাম্পল আউটপুট হবে এরকম:

```
PASS
ok      example.com/my/math 0.002s
```

টেবিল-চালিত টেস্টের জন্য, যা বিভিন্ন ইনপুট এবং আউটপুট সংমিশ্রণ দক্ষতার সাথে পরীক্ষা করতে দেয়, টেস্ট কেস প্রতিনিধিত্বকারী কাঠামোর একটি স্লাইস সংজ্ঞায়িত করুন:

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        expected int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testname := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testname, func(t *testing.T) {
            ans := Add(tt.x, tt.y)
            if ans != tt.expected {
                t.Errorf("got %d, want %d", ans, tt.expected)
            }
        })
    }
}
```

## গভীর দর্শন
Go টেস্টিং ফ্রেমওয়ার্ক, যা Go 1-এ ভাষাটির সাথে সাথে প্রবর্তন করা হয়েছিল, Go টুলচেইনের সাথে সহজে সংহত হওয়ার ডিজাইন করা হয়েছিল, যা Go-র সহজ এবং দক্ষ সফটওয়্যার উন্নয়নের উপর জোর দেয়। অন্যান্য ভাষার কিছু টেস্টিং ফ্রেমওয়ার্কের মতো বাহ্যিক লাইব্রেরি বা জটিল সেটআপের উপর নির্ভর না করে, Go-র নির্মিত `testing` প্যাকেজ টেস্ট লেখা এবং চালানীবিক উপায় প্রদান করে।

Go-র টেস্টিং পদ্ধতির এক আকর্ষণীয় দিক হল এটি যে কনভেনশন ওভার কনফিগারেশন নীতি গ্রহণ করে, যেমন ফাইল নেমিং প্যাটার্ন (`_test.go`) এবং বাহ্যিক নির্ভরতা অপেক্ষা স্ট্যান্ডার্ড লাইব্রেরি কার্যকারিতা ব্যবহারের মতো। এই সরল পদ্ধতি ডেভেলপারদের টেস্ট লেখার জন্য উৎসাহিত করে, যেহেতু প্রবেশের বাধা কম।

যদিও Go-র নির্মিত টেস্টিং সুবিধাগুলি অনেক ক্ষেত্রে ঢেকে রাখে, তবুও তৃতীয় পক্ষের সরঞ্জাম বা ফ্রেমওয়ার্কের কিছু ক্ষেত্রে আরও ফাংশনালিটি প্রদানের জন্য বিদ্যমান, যেমন মক জেনারেশন, ফাজ টেস্টিং, বা বিহেভিয়ার-চালিত ডেভেলপমেন্ট (BDD) শৈলীর টেস্ট। Testify বা GoMock এর মতো জনপ্রিয় লাইব্রেরিগুলি Go-র মানক টেস্টিং সামর্থ্যগুলির পরিপূরক, বিশেষ করে অনেক নির্ভরতাসমূহের জটিল অ্যাপ্লিকেশনের ক্ষেত্রে আরও প্রকাশকারী অ্যাসারশন বা মক জেনারেশন ক্ষমতা প্রদান করে।

এই বিকল্পগুলির অস্তিত্ব সত্ত্বেও, স্ট্যান্ডার্ড Go টেস্টিং প্যাকেজ Go ভাষায় টেস্টিংয়ের জন্য মূল ভিত্তি হিসেবে অবশিষ্ট থাকে এর সরলতা, কর্মক্ষমতা, এবং ভাষা ও টুলচেইনের সাথে ঘনিষ্ঠ সংযোগের কারণে। ডেভেলপাররা এটিকে তৃতীয় পক্ষের সরঞ্জামগুলির সাথে সম্পূরক করতে চায় কিনা, Go টেস্টিং ফ্রেমওয়ার্ক কোডের মান এবং নির্ভরযোগ্যতা নিশ্চিত করার জন্য একটি দৃঢ় ভিত্তি প্রদান করে।
