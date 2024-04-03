---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:17:12.671898-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: Go \u09A4\u09C7, `strings` \u09AA\
  \u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8\
  \ \u09AB\u09BE\u0982\u09B6\u09A8 \u09AA\u09CD\u09B0\u09B8\u09CD\u09A4\u09BE\u09AC\
  \ \u0995\u09B0\u09C7 \u09AF\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\
  \u09BC\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\u09BE\u09A8 \u098F\u09AC\u0982\
  \ \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\u09BE\u09AA\u09A8 \u0995\u09B0\
  \u09C7\u0964 \u0986\u09B8\u09C1\u09A8 \u0995\u09BF\u099B\u09C1 \u09B8\u09BE\u09A7\
  \u09BE\u09B0\u09A3 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u0985\u09A8\u09CD\u09AC\
  \u09C7\u09B7\u09A3\u2026"
lastmod: '2024-03-17T18:47:43.457330-06:00'
model: gpt-4-0125-preview
summary: "Go \u09A4\u09C7, `strings` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AC\
  \u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09AB\u09BE\u0982\u09B6\u09A8 \u09AA\u09CD\
  \u09B0\u09B8\u09CD\u09A4\u09BE\u09AC \u0995\u09B0\u09C7 \u09AF\u09BE \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982\u09AF\u09BC\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8 \u0995\u09B0\u09C7\u0964 \u0986\u09B8\u09C1\u09A8 \u0995\u09BF\
  \u099B\u09C1 \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u09AA\u09A6\u09CD\u09A7\u09A4\
  \u09BF \u0985\u09A8\u09CD\u09AC\u09C7\u09B7\u09A3 \u0995\u09B0\u09BF\u0964\n\n**`strings.Contains`\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u099F\u09C7\u0995\
  \u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\u09BE\u09A8:**."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
weight: 10
---

## কীভাবে:
Go তে, `strings` প্যাকেজ বিভিন্ন ফাংশন প্রস্তাব করে যা স্ট্রিংয়ের মধ্যে টেক্সট অনুসন্ধান এবং প্রতিস্থাপন করে। আসুন কিছু সাধারণ পদ্ধতি অন্বেষণ করি।

**`strings.Contains` ব্যবহার করে টেক্সট অনুসন্ধান:**

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	myString := "Hello, Go প্রোগ্রামাররা!"
	fmt.Println(strings.Contains(myString, "Go"))  // আউটপুট: true
	fmt.Println(strings.Contains(myString, "Java")) // আউটপুট: false
}
```

**`strings.Replace` এবং `strings.ReplaceAll` দ্বারা টেক্সট প্রতিস্থাপন:**

`strings.Replace` আপনাকে একটি স্ট্রিংয়ের মধ্যে সাবস্ট্রিংগুলি প্রতিস্থাপন করতে দেয়, প্রতিস্থাপনের সংখ্যা উল্লেখ করে, যেখানে `strings.ReplaceAll` সমস্ত নিদর্শন প্রতিস্থাপন করে।

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	myString := "Hello, Go! Go মজাদার।"
	fmt.Println(strings.Replace(myString, "Go", "Golang", 1))  // আউটপুট: Hello, Golang! Go মজাদার।
	fmt.Println(strings.ReplaceAll(myString, "Go", "Golang")) // আউটপুট: Hello, Golang! Golang মজাদার।
}
```

**জটিল অনুসন্ধান এবং প্রতিস্থাপনের জন্য `regexp` প্যাকেজ ব্যবহার:**

অধিক জটিল প্যাটার্নের জন্য, `regexp` প্যাকেজ খুবই শক্তিশালী, নিয়মিত এক্সপ্রেশন সমর্থন করে।

```go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	myString := "Hello, Go প্রোগ্রামাররা! Go মজাদার।"
	re := regexp.MustCompile(`Go`)
	fmt.Println(re.ReplaceAllString(myString, "Golang"))  // আউটপুট: Hello, Golang প্রোগ্রামাররা! Golang মজাদার।
}
```

## গভীর ডাইভ
Go তে, টেক্সট ম্যানিপুলেশন, অনুসন্ধান এবং প্রতিস্থাপন অপারেশন সহ, সহজ এবং কার্যকারী হওয়ার পরিকল্পনা করা হয়েছে, Go-র ব্যাপক স্ট্যান্ডার্ড লাইব্রেরি ব্যবহার করে। `strings` প্যাকেজ বেশিরভাগ সাধারণ ব্যবহারের ক্ষেত্রে উপযুক্ত মৌলিক কার্যকারিতা প্রদান করে, যখন `regexp` প্যাকেজ আরও জটিল প্যাটার্নগুলি, যা নিয়মিত এক্সপ্রেশন প্রয়োজন, পরিবেশন করে।

ইতিহাসগতভাবে, Go এর স্ট্রিং এবং টেক্সট ম্যানিপুলেশন হ্যান্ডলিংয়ে সাদাসিধা এবং কর্মক্ষমতা গুরুত্ব দিয়েছে। `strings` এবং `regexp` মতো শক্তিশালী প্যাকেজগুলি স্ট্যান্ডার্ড লাইব্রেরিতে অন্তর্ভুক্ত করার সিদ্ধান্ত ওয়েব ডেভেলপমেন্ট এবং টেক্সট প্রসেসিং অ্যাপ্লিকেশনের জন্য Go কে একটি বাস্তবিক বিকল্প হিসেবে করে তুলতে চেয়েছিল যেখানে এই ধরণের অপারেশন প্রচুর হয়।

মনে রাখা উচিত যে যদিও Go-র `strings` এবং `regexp` প্যাকেজ অনেক প্রয়োজন পূরণ করে, অন্যান্য ভাষা বা বিশেষায়িত লাইব্রেরি বিশেষ করে ইউনিকোড হ্যান্ডলিং বা প্রাকৃতিক ভাষা প্রসেসিং ক্ষেত্রে আরও উন্নত টেক্সট ম্যানিপুলেশন বৈশিষ্ট্য অফার করতে পারে। তবে, সফ্টওয়্যার ডেভেলপমেন্টে অধিকাংশ অনুসন্ধান এবং প্রতিস্থাপন কাজের জন্য, Go বাক্সের বাইরে দৃঢ় এবং কার্যকারী টুলস প্রদান করে।
