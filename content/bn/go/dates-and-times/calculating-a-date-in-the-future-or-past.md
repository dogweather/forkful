---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:08.119842-06:00
description: "Go \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC \u09AD\u09AC\u09BF\u09B7\u09CD\
  \u09AF\u09CE \u0985\u09A5\u09AC\u09BE \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09B9\u09BF\u09B8\u09BE\u09AC \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\
  \u09AF\u09BC\u09C7\u09B0 \u09AE\u09BE\u09A8\u0997\u09C1\u09B2\u09BF\u0995\u09C7\
  \ \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09C7 \u098F\u0995\
  \u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09AA\u09AF\
  \u09BC\u09C7\u09A8\u09CD\u099F \u09A8\u09BF\u09B0\u09CD\u09A3\u09AF\u09BC \u0995\
  \u09B0\u09BE \u09AF\u09C7\u099F\u09BF \u098F\u0995\u099F\u09BF \u09A6\u09C7\u0993\
  \u09AF\u09BC\u09BE \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.490944-06:00'
model: gpt-4-0125-preview
summary: "Go \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC \u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\
  \u09CE \u0985\u09A5\u09AC\u09BE \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u09A4\u09BE\
  \u09B0\u09BF\u0996 \u09B9\u09BF\u09B8\u09BE\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\
  \u09BC\u09C7\u09B0 \u09AE\u09BE\u09A8\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09AA\
  \u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09C7 \u098F\u0995\u099F\
  \u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09AA\u09AF\u09BC\
  \u09C7\u09A8\u09CD\u099F \u09A8\u09BF\u09B0\u09CD\u09A3\u09AF\u09BC \u0995\u09B0\
  \u09BE \u09AF\u09C7\u099F\u09BF \u098F\u0995\u099F\u09BF \u09A6\u09C7\u0993\u09AF\
  \u09BC\u09BE \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0\u2026"
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

Go ভাষায় ভবিষ্যৎ অথবা অতীতের তারিখ হিসাব করা মানে তারিখ এবং সময়ের মানগুলিকে পরিবর্তন করে একটি নির্দিষ্ট পয়েন্ট নির্ণয় করা যেটি একটি দেওয়া তারিখের তুলনায় সাপেক্ষে। যে সমস্ত অ্যাপ্লিকেশনগুলিতে সময়সূচি, শেষ তারিখ, অনুস্মারক, বা সময়ের অগ্রগতি বা প্রতিগমন মৌলিক হয়, প্রোগ্রামাররা সাধারণত এই কাজটি সম্পাদন করে থাকেন।

## কিভাবে:

Go তারিখ এবং সময় অপারেশনের জন্য `time` প্যাকেজ সরবরাহ করে, যা সময় যোগ অথবা বিয়োগের জন্য সরাসরি মেকানিজম অফার করে। `time` প্যাকেজ ব্যবহার করে ভবিষ্যৎ বা অতীতের তারিখ হিসাব করার একটি দৃষ্টান্ত এখানে:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// বর্তমান তারিখ Íবং সময়
	now := time.Now()
	fmt.Println("বর্তমান তারিখ এবং সময়: ", now)

	// ভবিষ্যত ১০ দিনের তারিখ হিসাব করা
	futureDate := now.AddDate(0, 0, 10)
	fmt.Println("ভবিষ্যতের ১০ দিন পরের তারিখ: ", futureDate)
	
	// অতীতের ৩০ দিন আগের তারিখ হিসাব করা
	pastDate := now.AddDate(0, 0, -30)
	fmt.Println("অতীতের ৩০ দিন আগের তারিখ: ", pastDate)
	
	// বর্তমান তারিখ এবং সময়ে ৫ ঘন্টা Íবং ৩০ মিনিট যোগ করা
	futureTime := now.Add(5*time.Hour + 30*time.Minute)
	fmt.Println("ভবিষ্যত সময় (৫ ঘন্টা Íবং ৩০ মিনিট পরে): ", futureTime)
}
```

নমুনা আউটপুট:
```
বর্তমান তারিখ এবং সময়:  2023-04-01 15:04:05.123456789 +0000 UTC
ভবিষ্যতের ১০ দিন পরের তারিখ:  2023-04-11 15:04:05.123456789 +0000 UTC
অতীতের ৩০ দিন আগের তারিখ:  2023-03-02 15:04:05.123456789 +0000 UTC
ভবিষ্যত সময় (৫ ঘন্টা Íবং ৩০ মিনিট পরে):  2023-04-01 20:34:05.123456789 +0000 UTC
```
লক্ষ্য করুন কিভাবে `AddDate` পদ্ধতিটি বছর, মাস, এবং দিন অনুযায়ী তারিখ পরিবর্তনের জন্য ব্যবহৃত হয়, এবং `Add` পদ্ধতিটি ঘন্টা, মিনিট, এবং সেকেন্ডের মতো আরও নির্দিষ্ট সময় ডেল্টার জন্য ব্�্যবহৃত হয়।

## গভীর ডুব

Go প্রোগ্রামিং ভাষার `time` প্যাকেজ সময় নিয়ন্ত্রণের জন্য শক্তিশালী টাইপ নিরাপত্তা এবং পরিষ্কার সিনট্যাক্স সহজলভ্�্য করে তোলে, যা Go এর জন্য প্রশংসিত। এর বাস্তবায়ন অধীনস্থ অপারেটিং সিস্টেম দ্বারা প্রদত্ত সময় নিয়ন্ত্রণ ক্ষমতাগুলিতে নির্ভর করে, দক্ষতা এবং সঠিকতা নিশ্চিত করে। ঐতিহাসিকভাবে, সময় এবং তারিখের মান প্রোগ্রামিংয়ে সমস্যাসংকুল ছিল সময় অঞ্চল, অধিবর্ষ, এবং দিনের আলো সঞ্চয় পরিবর্তনের বৈচিত্র্যের কারণে। Go এর `time` প্যাকেজ এই জটিলতা অনেকটা সরল করে, ডেভেলপারদের জন্য সময় নিয়ন্ত্রণের একটি দৃঢ় টুলকি�্ অফার করে।

যদিও Go এর নেটিভ `time` প্যাকেজ সময় নিয়ন্ত্রণের প্রায় সব চাহিদাকে সমর্থন করে, `github.com/jinzhu/now` এর মতো বিকল্প লাইব্রেরি নির্দিষ্ট ব্যবহারকারীর ক্ষেত্র জন্য অতি�্যন্ত সুবিধা �্ব কার্যকারিতা সরবরাহ করে। নেটিভ `time` প্যাকেজ দ্বারা সরাসরি সমর্থিত নয় এমন আরও জটিল �্যাট এবং সময় নিয়ন্ত্রণের চাহিদাগুলিতে এই বিকল্পগুলি �্যান্ত উপকারী হতে পারে।

তবে, বেশিরভাগ অ্যাপ্লিকেশনের জন্য, Go এর অন্তর্নির্মিত সময় নিয়ন্ত্রণ ক্ষামতা একটি দৃঢ় ভিত্তি সরবরাহ করে। তারা �্যান্তিমের প্যাকেজের জন্য না গিয়েও ডেভেলপারদের বেশিরভাগ সাধারণ সময�্ক্রিয়াগুলি দক্ষতার সাথে সামলানোর নিশ্চয়তা দেয়, ব্যবহারে সহজলভ্যতা �্ব কর্মক্ষমতার �্যান্তুলন সৃষ্টি করে।