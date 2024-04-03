---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:46.754485-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Go-\u09A4\u09C7, \u09A8\u09BF\u09B0\
  \u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A8\u09BF\u09A6\u09B0\u09CD\u09B6\u09A8\u09C7\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AE\u09BF\u09B2\u09C7 \u09AF\u09BE\u09DF \u098F\
  \u09AE\u09A8 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\
  \u099B\u09C7 \u09AB\u09C7\u09B2\u09BE `regexp` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\
  \u099C \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09B9\
  \u099C\u09C7\u0987 \u09B8\u09AE\u09CD\u09AA\u09A8\u09CD\u09A8 \u0995\u09B0\u09BE\
  \ \u09AF\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7, \u0986\u09AE\u09B0\u09BE \u0995\u09C0\u09AD\u09BE\u09AC\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.456122-06:00'
model: gpt-4-0125-preview
summary: "Go-\u09A4\u09C7, \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F\
  \ \u09A8\u09BF\u09A6\u09B0\u09CD\u09B6\u09A8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u09AE\u09BF\u09B2\u09C7 \u09AF\u09BE\u09DF \u098F\u09AE\u09A8 \u0985\u0995\u09CD\
  \u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE `regexp` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09B9\u099C\u09C7\u0987 \u09B8\u09AE\
  \u09CD\u09AA\u09A8\u09CD\u09A8 \u0995\u09B0\u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7, \u0986\u09AE\u09B0\u09BE\
  \ \u0995\u09C0\u09AD\u09BE\u09AC\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u09A5\u09C7\u0995\u09C7 \u09B8\u09AC \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\
  \u09C1\u09B2\u09BF, \u098F\u09B0\u09AA\u09B0\u09C7 \u09B8\u09AC \u0985-\u0985\u09B2\
  \u09CD\u09AB\u09BE\u09A8\u09BF\u0989\u09AE\u09C7\u09B0\u09BF\u0995 \u0985\u0995\u09CD\
  \u09B7\u09B0 \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09A4\u09C7 \u09B9\u09DF\
  \ \u09A4\u09BE \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09B9\u09BF\u09B8\u09BE\u09AC\
  \u09C7 \u09A6\u09C7\u0996\u09BE\u09AC\u0964\n\n1."
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কিভাবে:
Go-তে, নির্দিষ্ট নিদর্শনের সাথে মিলে যায় এমন অক্ষরগুলি মুছে ফেলা `regexp` প্যাকেজ ব্যবহার করে সহজেই সম্পন্ন করা যেতে পারে। এখানে, আমরা কীভাবে স্ট্রিং থেকে সব সংখ্যাগুলি, এরপরে সব অ-অল্ফানিউমেরিক অক্ষর মুছে ফেলতে হয় তা উদাহরণ হিসাবে দেখাব।

1. **সব সংখ্যা মুছে ফেলা:**

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go1 কুল, কিন্তু Go2 আরও কুলার হবে! এখন: 2023."
	
    // সংখ্যার জন্য নিয়মিত প্রকাশনার সংকলন করুন
    re, err := regexp.Compile("[0-9]+")
    if err != nil {
        fmt.Println("রেজেক্স সংকলনে ত্রুটি:", err)
        return
    }
	
    // সংখ্যাগুলি একটি খালি স্ট্রিং দ্বারা প্রতিস্থাপন করুন
    result := re.ReplaceAllString(text, "")
	
    fmt.Println(result) // আউটপুট: Go কুল, কিন্তু Go আরও কুলার হবে! এখন: .
}
```

2. **সব অ-অল্ফানিউমেরিক অক্ষর মুছে ফেলা:**

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go হল #1 @ প্রোগ্রামিং ভাষাগুলিতে!"
	
    // অ-অল্ফানিউমেরিক অক্ষরগুলির জন্য নিয়মিত প্রকাশনার সংকলন করুন
    re, err := regexp.Compile("[^a-zA-Z0-9]+")
    if err != nil {
        fmt.Println("রেজেক্স সংকলনে ত্রুটি:", err)
        return
    }
	
    // অ-অল্ফানিউমেরিক অক্ষরগুলি একটি খালি স্ট্রিং দ্বারা প্রতিস্থাপন করুন
    result := re.ReplaceAllString(text, "")
	
    fmt.Println(result) // আউটপুট: Gois1programminglanguages
}
```

## গভীর ডুব
`regexp` প্যাকেজ Go-তে নিয়মিত প্রকাশনার সাথে প্যাটার্ন মিল এবং ম্যানিপুলেশনের জন্য একটি শক্তিশালী ইন্টারফেস প্রদান করে। এটির বাস্তবায়ন RE2, একটি নিয়মিত প্রকাশনা লাইব্রেরি থেকে উদ্ভূত, যা রৈখিক সময় নির্বাহের গ্যারান্টি দেয়, অন্য কিছু regex ইঞ্জিনে উপস্থিত "বিপর্যয়কর ব্যাকট্র্যাকিং" ইস্যু এড়াতে সক্ষম করে। এটি Go-র regex-কে বহুবিধ অ্যাপ্লিকেশনের জন্য অপেক্ষাকৃত নিরাপদ এবং দক্ষ করে তোলে।

যদিও `regexp` প্যাকেজ প্যাটার্নের সাথে মোকাবেলা করার জন্য একটি সম্পূর্ণ সমাধান, সাধারণ বা খুব নির্দিষ্ট স্ট্রিং ম্যানিপুলেশনের জন্য, অন্যান্য স্ট্রিং ফাংশন যেমন `strings.Replace()`, `strings.Trim()`, বা স্লাইসিং আরও কার্যকরী বিকল্প অফার করতে পারে। নিয়মিত প্রকাশনা একটি শক্তিশালী টুল, তবে তাদের আপেক্ষিক গণনামূলক ব্যয় মানে এমন অপারেশনের জন্য যা অবজ্ঞা ছাড়াই নির্দিষ্ট করা যেতে পারে, মানক লাইব্রেরির বিকল্পগুলি অন্বেষণ করা কখনও কখনও সহজ এবং আরও দক্ষ কোড প্রদান করতে পারে।
