---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:50.275900-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Go-\u09A4\u09C7, \u09B0\u200D\u09CD\
  \u09AF\u09BE\u09A8\u09CD\u09A1\u09AE \u09B8\u0982\u0996\u09CD\u09AF\u09BE `math/rand`\
  \ \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u09AA\u09CD\u09B0\u09A4\u09BF\u09A8\u09BF\u09A7\u09BF\
  \ \u09B0\u200D\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09AE \u09B8\u0982\u0996\u09CD\
  \u09AF\u09BE\u0997\u09C1\u09B2\u09BF \u0989\u09CE\u09AA\u09A8\u09CD\u09A8 \u0995\
  \u09B0\u09BE \u09B9\u09AF\u09BC \u0985\u09A5\u09AC\u09BE `crypto/rand` \u0995\u09CD\
  \u09B0\u09BF\u09AA\u09CD\u099F\u09CB\u0997\u09CD\u09B0\u09BE\u09AB\u09BF\u0995\u09AD\
  \u09BE\u09AC\u09C7\u2026"
lastmod: '2024-04-05T21:53:51.400415-06:00'
model: gpt-4-0125-preview
summary: "Go-\u09A4\u09C7, \u09B0\u200D\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09AE\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE `math/rand` \u09AA\u09CD\u09AF\u09BE\u0995\
  \u09C7\u099C \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09AA\
  \u09CD\u09B0\u09A4\u09BF\u09A8\u09BF\u09A7\u09BF \u09B0\u200D\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u09A1\u09AE \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\u09B2\u09BF\
  \ \u0989\u09CE\u09AA\u09A8\u09CD\u09A8 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC \u0985\
  \u09A5\u09AC\u09BE `crypto/rand` \u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AB\u09BF\u0995\u09AD\u09BE\u09AC\u09C7 \u09A8\u09BF\u09B0\
  \u09BE\u09AA\u09A6 \u09AA\u09CD\u09B0\u09A4\u09BF\u09A8\u09BF\u09A7\u09BF \u09B0\
  \u200D\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09AE \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \u0997\u09C1\u09B2\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF\u0964 \u099A\u09B2\u09C1\
  \u09A8 \u0989\u09AD\u09AF\u09BC\u0995\u09C7\u0987 \u0996\u09A8\u09A8 \u0995\u09B0\
  \u09BF\u0964."
title: "\u098F\u09B2\u09CB\u09AE\u09C7\u09B2\u09CB \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u0989\u09CE\u09AA\u09A8\u09CD\u09A8 \u0995\u09B0\u09BE"
weight: 12
---

## কিভাবে:
Go-তে, র‍্যান্ডম সংখ্যা `math/rand` প্যাকেজ ব্যবহার করে প্রতিনিধি র‍্যান্ডম সংখ্যাগুলি উৎপন্ন করা হয় অথবা `crypto/rand` ক্রিপ্টোগ্রাফিকভাবে নিরাপদ প্রতিনিধি র‍্যান্ডম সংখ্যাগুলির জন্য। চলুন উভয়কেই খনন করি।

### `math/rand` ব্যবহার করে প্রতিনিধি র‍্যান্ডম সংখ্যা
প্রথমে, `math/rand` প্যাকেজটি এবং `time` প্যাকেজটি ইম্পোর্ট করুন জেনারেটরকে সীড দিতে। সীডিং নিশ্চিত করে যে প্রতি রানে আপনি একটি ভিন্ন সিকোয়েন্স পাবেন।

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("A random number:", rand.Intn(100)) // Generates a number between 0 and 99
}
```

নমুনা আউটপুট: `A random number: 42`

### `crypto/rand` ব্যবহার করে ক্রিপ্টোগ্রাফিকভাবে নিরাপদ প্রতিনিধি র‍্যান্ডম সংখ্যা
আরো নিরাপত্তা-সংবেদনশীল অ্যাপ্লিকেশনের জন্য, `crypto/rand` প্যাকেজ উপযুক্ত কারণ এটি র‍্যান্ডম সংখ্যা উৎপন্ন করে যা ভবিষ্যদ্বাণী করা কঠিন, যা এটিকে ক্রিপ্টোগ্রাফিক অপারেশনের জন্য উপযুক্ত করে তোলে।

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("A secure random number:", n)
}
```

নমুনা আউটপুট: `A secure random number: 81`

## গভীর ডুব
`math/rand` এবং `crypto/rand` প্যাকেজের মধ্যে মূল পার্থক্য Go-তে তাদের এন্ট্রপির উৎস এবং তাদের প্রয়োগের ক্ষেত্র থেকে আসে। `math/rand` একটি প্রারম্ভিক সীড ভিত্তিক প্রতিনিধি র‍্যান্ডম সংখ্যা উৎপন্ন করে; এর মানে হল যে সিকোয়েন্সটি নির্ধারিত এবং সীড জানা থাকলে ভবিষ্যদ্বাণী করা সম্ভব। এটি উচ্চ কর্মক্ষমতা এবং পূর্ণ অনিশ্চয়তার দরকার নেই, এমন সিনারিওগুলির জন্য উপযুক্ত, যেমন সিমুলেশন বা গেমস।

অন্যদিকে, `crypto/rand` মৌলিক অপারেটিং সিস্টেম থেকে এন্ট্রপি উত্তোলন করে, যা অপ্রত্যাশিততা জরুরী এমন ক্রিপ্টোগ্রাফিক ব্যবহারের জন্য উপযুক্ত। তবে, এটি কর্মক্ষমতা এবং এর দ্বারা উৎপন্ন সংখ্যাগুলি সম্বলিত করার জটিলতার মানে, যেমন `*big.Int` টাইপের জন্য পূর্ণসংখ্যা নিয়ে ব্যবহার করা।

ঐতিহাসিকভাবে, কম্পিউটারগুলিতে র্যান্ডম সংখ্যা উৎপন্নের ধারণাটি সর্বদা প্রকৃত "র‍্যান্ডমনেস" এর ধারের উপর নৃত্য করেছে, যেখানে প্রারম্ভিক সিস্টেমগুলি প্রধানত র‍্যান্ডমনেস অনুকরণ করা নির্ধারিত অ্যালগরিদমের উপর নির্ভর করেছিল। কম্পিউটার বিকাশের সাথে সাথে, এই অ্যালগরিদমগুলি তাদের পরিবেশ থেকে আরও জটিল এন্ট্রপির উৎস অন্তর্ভুক্তি করে বিকশিত হয়েছে।

এই অগ্রগতি সত্ত্বেও, কম্পিউটারগুলির নিজস্ব নির্ধারিত প্রকৃতির দিক থেকে কম্পিউটিং এ পূর্ণ র‍্যান্ডমনেস অন্বেষণ অন্তঃসারশূন্যভাবে প্যারাডক্সিকাল। এটি কেন অধিকাংশ অ্যাপ্লিকেশনের জন্য যেখানে প্রত্যাশিততা ক্ষতিকর হতে পারে, `crypto/rand` এর মত উৎস থেকে ক্রিপ্টোগ�
