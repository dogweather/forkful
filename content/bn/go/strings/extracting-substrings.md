---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:49.394779-06:00
description: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\
  \u09CD\u09B8\u099F\u09CD\u09B0\u09CD\u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982\u09DF\u09C7\u09B0 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\
  \u09CD\u099F \u0985\u0982\u09B6 \u09A4\u09BE\u09A6\u09C7\u09B0 \u0985\u09AC\u09B8\
  \u09CD\u09A5\u09BE\u09A8 \u0985\u09A8\u09C1\u09AF\u09BE\u09DF\u09C0 \u09AA\u09C1\
  \u09A8\u09B0\u09C1\u09A6\u09CD\u09A7\u09BE\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3 \u09AA\u09CD\
  \u09B0\u09BE\u09DF\u0987 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09A1\u09C7\u099F\
  \u09BE \u09A6\u0995\u09CD\u09B7\u09A4\u09BE\u09B0 \u09B8\u09BE\u09A5\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.461904-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\
  \u09CD\u09B8\u099F\u09CD\u09B0\u09CD\u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982\u09DF\u09C7\u09B0 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\
  \u09CD\u099F \u0985\u0982\u09B6 \u09A4\u09BE\u09A6\u09C7\u09B0 \u0985\u09AC\u09B8\
  \u09CD\u09A5\u09BE\u09A8 \u0985\u09A8\u09C1\u09AF\u09BE\u09DF\u09C0 \u09AA\u09C1\
  \u09A8\u09B0\u09C1\u09A6\u09CD\u09A7\u09BE\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3 \u09AA\u09CD\
  \u09B0\u09BE\u09DF\u0987 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09A1\u09C7\u099F\
  \u09BE \u09A6\u0995\u09CD\u09B7\u09A4\u09BE\u09B0 \u09B8\u09BE\u09A5\u09C7\u2026"
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

সাবস্ট্রিং এক্সট্র্যাক্ট করা মানে হল একটি স্ট্রিংয়ের নির্দিষ্ট অংশ তাদের অবস্থান অনুযায়ী পুনরুদ্ধার করা। প্রোগ্রামারগণ প্রায়ই টেক্সট ডেটা দক্ষতার সাথে প্রক্রিয়া করা বা ম্যানিপুলেট করার জন্য এই অপারেশনটি সম্পাদন করে, যেমন ইনপুট পার্সিং, ফরম্যাট ভ্যালিডেশন বা আউটপুট প্রস্তুত করা।

## কিভাবে:

Go ভাষায়, `string` টাইপটি বাইটের একটি রিড-ওনলি স্লাইস। সাবস্ট্রিংগুলি এক্সট্র্যাক্ট করার জন্য, মূলত `slice` সিনট্যাক্সের পাশাপাশি লেন্থ চেকিংয়ের জন্য বিল্ট-ইন `len()` ফাংশন এবং আরও জটিল অপারেশনগুলির জন্য `strings` প্যাকেজের ব্যবহার করা হয়। এখানে আপনি কিভাবে এটি অর্জন করতে পারেন তার একটি উদাহরণ:

### বেসিক স্লাইসিং

```go
package main

import (
    "fmt"
)

func main() {
    str := "Hello, World!"
    // "World" এক্সট্র্যাক্ট করা হচ্ছে
    subStr := str[7:12]
    
    fmt.Println(subStr) // আউটপুট: World
}
```

### `strings` প্যাকেজ ব্যবহার করে

আরও উন্নত সাবস্ট্রিং এক্সট্র্যাকশনের জন্য, যেমন একটি নির্দিষ্ট সাবস্ট্রিংয়ের পরে বা আগে স্ট্রিং এক্সট্র্যাক্ট করা, আপনি `strings` প্যাকেজ ব্যবহার করতে পারেন।

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "name=John Doe"
    // "=" এর পরে সাবস্ট্রিং এক্সট্র্যাক্ট করা হচ্ছে
    subStr := strings.SplitN(str, "=", 2)[1]
    
    fmt.Println(subStr) // আউটপুট: John Doe
}
```

মনে রাখা প্রয়োজন যে Go এর স্ট্রিংগুলি UTF-8 এনকোডেড এবং যদি তারা মাল্টি-বাইট অক্ষর অন্তর্ভুক্ত করে তবে সরাসরি বাইট স্লাইস সবসময় বৈধ স্ট্রিং ফলাফল নাও দিতে পারে। ইউনিকোড সমর্থনের জন্য, `range` বা `utf8` প্যাকেজের ব্যবহার বিবেচনা করতে হবে।

### ইউনিকোড অক্ষর সম্ভালানো

```go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "Hello, 世界"
    // ইউনিকোড অক্ষর বিবেচনা করে সাবস্ট্রিং খুঁজছে
    runeStr := []rune(str)
    subStr := string(runeStr[7:])
    
    fmt.Println(subStr) // আউটপুট: 世界
}
```

## গভীর ডুব

Go তে সাবস্ট্রিং এক্সট্র্যাক্ট করা অত্যন্ত সহজ, এর স্লাইস সিনট্যাক্স এবং সম্পূর্ণ স্ট্যান্ডার্ড লাইব্রেরির কৃতিত্বে। ঐতিহাসিকভাবে, পূর্ববর্তী প্রোগ্রামিং ভাষাগুলিতে এ ধরনের টেক্সট ম্যানিপুলেশন সামলানোর জন্য আরও সরাসরি ফাংশন বা মেথড প্রদান করা হয়েছিল। তবে, Go এর পদ্ধতি নিরাপত্তা এবং দক্ষতা প্রাধান্য দেয়, বিশেষ করে এর অপরিবর্তনীয় স্ট্রিং এবং ইউনিকোড অক্ষরগুলির স্পষ্ট হ্যান্ডলিং যা রুনস মাধ্যমে সম্পাদন করা যায়।

যদিও সোজা স্লাইসিং পারফরম্যান্স দক্ষতা থেকে উপকৃত হয়, এটি সরাসরি UTF-8 অক্ষরগুলি হ্যান্ডেল করার জটিলতা বহন করে। `rune` টাইপের প্রবর্তনে Go প্রোগ্রামের নিরাপদে ইউনিকোড টেক্সট হ্যান্ডল করা সম্ভব হয়ে উঠেছে, যা আন্তর্জাতিক অ্যাপ্লিকেশনের জন্য একটি শক্তিশালী বিকল্প তৈরি করে।

তাছাড়া, অন্যান্য ভাষার প্রোগ্রামারগণ হয়ত বিল্ট-ইন উচ্চ-স্তরের স্ট্রিং ম্যানিপুলেশন ফাংশনগুলি অনুপস্থিত বোধ করতে পারেন। তবে, Go এর স্ট্যান্ডার্ড লাইব্রেরীর `strings` এবং `bytes` প্যাকেজগুলিতে একটি সমৃদ্ধ ফাংশনের সেট অফার করা হয়েছে যা, যদিও একটু বেশি বয়লারপ্লেট প্রয়োজন, তবে সাবস্ট্রিং এক্সট্র্যাকশন সহ স্ট্রিং প্রসেসিংয়ের জন্য শক্তিশালী বিকল্প প্রদান করে।

সারাংশে, Go এর স্ট্রিং ম্যানিপুলেশনের চারপাশের ডিজাইন পছন্দগুলি এর লক্ষ্যগুলি প্রতিফলিত করে - আধুনিক, আন্তর্জাতিকীকৃত টেক্সট ডেটা নিয়ে কাজ করার ক্ষেত্রে সাধারণতা, দক্ষতা, এবং নিরাপত্তা। যদিও এটি একটু পরিবর্তন প্রয়োজন হতে পারে, Go সাবস্ট্রিং এক্সট্র্যাকশন এবং আরও অনেকের হ্যান্ডলিং জন্য কার্যকর এবং দক্ষ টুলস অফার করে।