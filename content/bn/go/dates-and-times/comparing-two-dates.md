---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:47.539499-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Go-\u09A4\u09C7, \u09A4\u09BE\u09B0\
  \u09BF\u0996\u0997\u09C1\u09B2\u09BF \u09AA\u09CD\u09B0\u09A7\u09BE\u09A8\u09A4\
  \ `time` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09A5\u09C7\u0995\u09C7 `time.Time`\
  \ \u099F\u09BE\u0987\u09AA \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09BF\u09A4 \u09B9\u09AF\u09BC\u0964 \u09A6\u09C1\u0987\u099F\
  \u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\
  \u09A4\u09C7, \u0986\u09AE\u09B0\u09BE `time.Time` \u099F\u09BE\u0987\u09AA \u09A6\
  \u09CD\u09AC\u09BE\u09B0\u09BE \u09AA\u09CD\u09B0\u09A6\u09A4\u09CD\u09A4\u2026"
lastmod: '2024-03-17T18:47:43.489882-06:00'
model: gpt-4-0125-preview
summary: "Go-\u09A4\u09C7, \u09A4\u09BE\u09B0\u09BF\u0996\u0997\u09C1\u09B2\u09BF\
  \ \u09AA\u09CD\u09B0\u09A7\u09BE\u09A8\u09A4 `time` \u09AA\u09CD\u09AF\u09BE\u0995\
  \u09C7\u099C \u09A5\u09C7\u0995\u09C7 `time.Time` \u099F\u09BE\u0987\u09AA \u09A6\
  \u09CD\u09AC\u09BE\u09B0\u09BE \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09BF\u09A4\
  \ \u09B9\u09AF\u09BC\u0964 \u09A6\u09C1\u0987\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\u09A4\u09C7, \u0986\u09AE\u09B0\
  \u09BE `time.Time` \u099F\u09BE\u0987\u09AA \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE\
  \ \u09AA\u09CD\u09B0\u09A6\u09A4\u09CD\u09A4 `Before()`, `After()`, \u098F\u09AC\
  \u0982 `Equal()` \u09AE\u09C7\u09A5\u09A1\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09BF\u0964\
  \ \u099A\u09B2\u09C1\u09A8 \u09A6\u09C1\u0987\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\
  \u0996\u09C7\u09B0 \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09BF\u09AD\u09BE\u09AC\
  \u09C7 \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC \u09A4\u09BE \u09A6\u09C7\u0996\
  \u09BE\u09A8\u09CB \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\u09C7 \u09A1\u09C1\u09AC\
  \ \u09A6\u09BF\u0987."
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কিভাবে:
Go-তে, তারিখগুলি প্রধানত `time` প্যাকেজ থেকে `time.Time` টাইপ দ্বারা পরিচালিত হয়। দুইটি তারিখ তুলনা করতে, আমরা `time.Time` টাইপ দ্বারা প্রদত্ত `Before()`, `After()`, এবং `Equal()` মেথডগুলি ব্যবহার করতে পারি। চলুন দুইটি তারিখের তুলনা কিভাবে করা যায় তা দেখানো উদাহরণে ডুব দিই:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// তুলনার জন্য দুই তারিখ পারস করা হচ্ছে
	dateStr1 := "2023-04-01"
	dateStr2 := "2023-04-15"
	date1, _ := time.Parse("2006-01-02", dateStr1)
	date2, _ := time.Parse("2006-01-02", dateStr2)

	// দুই তারিখের তুলনা করা হচ্ছে
	if date1.Before(date2) {
		fmt.Println(date1.Format("January 2, 2006"), "তারিখটি", date2.Format("January 2, 2006"), "এর আগে")
	} else if date1.After(date2) {
		fmt.Println(date1.Format("January 2, 2006"), "তারিখটি", date2.Format("January 2, 2006"), "এর পরে")
	} else {
		fmt.Println(date1.Format("January 2, 2006"), "ও", date2.Format("January 2, 2006"), "একই তারিখ")
	}
}
```

নমুনা আউটপুট:
```
এপ্রিল 1, 2023 তারিখটি এপ্রিল 15, 2023 এর আগে
```

এই প্রোগ্রামটি দেখায় কিভাবে স্ট্রিং থেকে তারিখগুলি পার্স করা, একটি সাধারণ প্রয়োজনীয়তা, এবং তারপরে `Before()`, `After()`, এবং `Equal()` মেথডগুলি ব্যবহার করে তারিখগুলি তুলনা করা যায়। এখানে `time.Parse()` মেথডটি Go-এর রেফারেন্স ডেট ফর্ম্যাট `"2006-01-02"` স্ট্রিং দিয়ে ব্যবহৃত হয়েছে।

## গভীরে ডুব:
Go প্রোগ্রামিং ভাষাতে, `time` প্যাকেজের ডিজাইন, যার মধ্যে `time.Time` টাইপ অন্তর্ভুক্ত, সুন্দর কিন্তু শক্তিশালী একটি মানক লাইব্রেরিতে সরবরাহ করার দর্শন প্রতিফলিত করে। `Before()`, `After()`, এবং `Equal()` মেথডগুলি `time.Time` এর তুলনা করা শুধুমাত্র সোজা করেছে না, বরং পঠনযোগ্যও করেছে, যা Go-এর পরিষ্কার ও সংক্ষিপ্ত কোডের উপর জোর দেয়।

ইতিহাসগতভাবে, প্রোগ্রামিং ভাষাগুলিতে তারিখ এবং সময় পরিচালনা করা সময় অঞ্চলের পার্থক্য, লাফিয়ে সেকেন্ড, এবং ক্যালেন্ডার সিস্টেমের প্রভেদের জন্য জটিলতা সহ হয়েছে। Go-এর `time` প্যাকেজ হচ্ছে অন্যান্য ভাষাগুলিতে তারিখ-সময়ের বাস্তবায়নের পিটফলস এবং সাফল্য থেকে শিক্ষা গ্রহণ করে একটি সম্পূর্ণ সমাধা�
