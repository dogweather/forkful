---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:13:11.618741-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Go \u098F\u0995\u099F\u09BF \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0995\u09CB\u099F\
  \u09C7\u09B6\u09A8 \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09AC\u09C7\u09B6 \u0995\u09BF\u099B\u09C1 \u09AA\u09A6\u09CD\
  \u09A7\u09A4\u09BF \u0985\u09AB\u09BE\u09B0 \u0995\u09B0\u09C7, \u0995\u09BF\u09A8\
  \u09CD\u09A4\u09C1 \u09B8\u09AC\u099A\u09C7\u09DF\u09C7 \u09B8\u09B0\u09B2 \u09AA\
  \u09A6\u09CD\u09A7\u09A4\u09BF\u099F\u09BF \u09B9\u09B2 `strings` \u09AA\u09CD\u09AF\
  \u09BE\u0995\u09C7\u099C \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u09AA\u09CD\u09B0\
  \u09A6\u09BE\u09A8\u0995\u09C3\u09A4 `Trim` \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:43.460848-06:00'
model: gpt-4-0125-preview
summary: "Go \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\
  \u09C7\u0995\u09C7 \u0995\u09CB\u099F\u09C7\u09B6\u09A8 \u09AE\u09C1\u099B\u09C7\
  \ \u09AB\u09C7\u09B2\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09C7\u09B6 \u0995\
  \u09BF\u099B\u09C1 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u0985\u09AB\u09BE\u09B0\
  \ \u0995\u09B0\u09C7, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u09B8\u09AC\u099A\u09C7\
  \u09DF\u09C7 \u09B8\u09B0\u09B2 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u099F\u09BF\
  \ \u09B9\u09B2 `strings` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09A6\u09CD\
  \u09AC\u09BE\u09B0\u09BE \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8\u0995\u09C3\u09A4\
  \ `Trim` \u098F\u09AC\u0982 `TrimFunc` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u0964 \u098F\u099F\u09BF \u0995\
  \u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09A4\u09C7 \u09B9\u09DF \u09A4\u09BE\
  \ \u09A8\u09BF\u099A\u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09DF\u09C7\
  \u099B\u09C7."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\
  \u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE"
weight: 9
---

## কিভাবে:
Go একটি স্ট্রিং থেকে কোটেশন মুছে ফেলার জন্য বেশ কিছু পদ্ধতি অফার করে, কিন্তু সবচেয়ে সরল পদ্ধতিটি হল `strings` প্যাকেজ দ্বারা প্রদানকৃত `Trim` এবং `TrimFunc` ফাংশন ব্যবহার করা। এটি কিভাবে করতে হয় তা নিচে দেখানো হয়েছে:

```go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	quotedString := `"This is a 'quoted' string"`

	// নির্দিষ্ট কোটেশন মুছে ফেলার জন্য strings.Trim ব্যবহার
	unquoted := strings.Trim(quotedString, `"'`)
	fmt.Println("Using strings.Trim:", unquoted)

	// আরও নিয়ন্ত্রণের জন্য strings.TrimFunc ব্যবহার করে কাস্টম পদ্ধতি
	unquotedFunc := strings.TrimFunc(quotedString, func(r rune) bool {
		return r == '"' || r == '\''
	})
	fmt.Println("Using strings.TrimFunc:", unquotedFunc)
}
```

এই উদাহরণ দেখাচ্ছে উভয় ডবল (`"`) এবং সিঙ্গেল (`'`) কোটেশন মুছে ফেলার দুইটি পদ্ধতি। `strings.Trim` ফাংশনটি সরল এবং আপনি যখন ঠিক থাকেন কি অক্ষর মুছে দিতে হবে সে ক্ষেত্রে ভালো কাজ করে। অন্যদিকে, `strings.TrimFunc` আরও নমনীয়তা প্রদান করে, আপনাকে একটি কাস্টম ফাংশন নির্ধারণ করতে দেয় যা সিদ্ধান্ত নেয় কোন অক্ষর মুছে দেওয়া হবে। উপরের কোডের নমুনা আউটপুট হল:

```
Using strings.Trim: This is a 'quoted' string
Using strings.TrimFunc: This is a 'quoted' string
```

উভয় পদ্ধতিই স্ট্রিং থেকে শুরুর এবং শেষের কোটেশন দক্ষতাপূর্ণভাবে মুছে ফেলে।

## গভীর আলোচনা
`strings` প্যাকেজের `Trim` এবং `TrimFunc` ফাংশনগুলি Go-র ব্যাপক স্ট্যান্ডার্ড লাইব্রেরির অংশ, যা তৃতীয়-পক্ষের প্যাকেজ ছাড়াই শক্তিশালী, তবে সরল স্ট্রিং ম্যানিপুলেশন সামর্থ্য প্রদান করার জন্য ডিজাইন করা হয়েছে। নেটওয়ার্ক সার্ভার এবং ডাটা পার্সারে Go-র প্রাথমিক মনোযোগের কারণে স্ট্রিং প্রক্রিয়াজাত করা একটি প্রচলিত কাজ, তাই স্ট্রিং দক্ষতার সাথে ম্যানেজ এবং ম্যানিপুলেট করার প্রয়োজন।

এই ফাংশনগুলির একটি উল্লেখযোগ্য দিক হল এগুলির রুনগুলির উপর ভিত্তি করে বাস্তবায়ন (Go-র Unicode কোড পয়েন্টের প্রতিনিধিত্ব)। এই ডিজাইন এগুলিকে বহু-বাইট অক্ষর যুক্ত স্ট্রিং সহজে হ্যান্ডল করতে দেয়, যা Go-র স্ট্রিং ম্যানিপুলেশন পদ্ধতিকে দৃঢ় এবং Unicode-বান্ধব করে।

কোটেশন মুছে ফেলার জন্য `Trim` এবং `TrimFunc`-এর সরাসরি ব্যবহার Go-তে সুবিধাজনক এবং ইডিওম্যাটিক, তারপরেও এটি উল্লেখ করা যেতে পারে যে আরও জটিল স্ট্রিং প্রক্রিয়াজাত কাজের জন্য (যেমন, নেস্টেড কোটেশন, এসকেপড কোটেশন), নিয়মিত এক্সপ্রেশন (`regexp` প্যাকেজের মাধ্যমে) বা ম্যানুয়াল পার্সিং ভালো সমাধান দিতে পারে। তবে, এই বিকল্পগুলির জটিলতা এবং পারফরম্যান্স বিবেচনার সাথে আসে। সুতরাং, সাধারণ কোটেশন মুছে ফেলার ক্ষেত্রে, দেখানো পদ্ধতিগুলি সাদাসিধা, পারফরম্যান্স এবং কার্যকারিতা এর মধ্যে একটি ভালো সমতা সরবরাহ করে।
