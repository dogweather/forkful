---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:11.747029-06:00
description: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u0987\u098F\u0995\u09CD\u09B8\u099F\u09C7\u09A8\
  \u09B8\u09BF\u09AC\u09B2 \u09AE\u09BE\u09B0\u09CD\u0995\u0986\u09AA \u09B2\u09CD\
  \u09AF\u09BE\u0999\u09CD\u0997\u09C1\u09AF\u09BC\u09C7\u099C\u09C7 \u0997\u09A0\u09BF\
  \u09A4 \u09A1\u09C7\u099F\u09BE \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09BE, \u09AF\u09BE \u09A1\u09C7\u099F\u09BE \u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0993 \u0985\u09CD\u09AF\u09BE\u0995\u09CD\
  \u09B8\u09C7\u09B8 \u0995\u09B0\u09BE \u099C\u09A1\u09BC\u09BF\u09A4\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0985\u09A8\
  \u09CD\u09AF\u09BE\u09A8\u09CD\u09AF\u2026"
lastmod: '2024-03-17T18:47:44.303955-06:00'
model: gpt-4-0125-preview
summary: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u0987\u098F\u0995\u09CD\u09B8\u099F\u09C7\u09A8\
  \u09B8\u09BF\u09AC\u09B2 \u09AE\u09BE\u09B0\u09CD\u0995\u0986\u09AA \u09B2\u09CD\
  \u09AF\u09BE\u0999\u09CD\u0997\u09C1\u09AF\u09BC\u09C7\u099C\u09C7 \u0997\u09A0\u09BF\
  \u09A4 \u09A1\u09C7\u099F\u09BE \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09BE, \u09AF\u09BE \u09A1\u09C7\u099F\u09BE \u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0993 \u0985\u09CD\u09AF\u09BE\u0995\u09CD\
  \u09B8\u09C7\u09B8 \u0995\u09B0\u09BE \u099C\u09A1\u09BC\u09BF\u09A4\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0985\u09A8\
  \u09CD\u09AF\u09BE\u09A8\u09CD\u09AF\u2026"
title: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
XML এর সাথে কাজ করা মানে ইএক্সটেনসিবল মার্কআপ ল্যাঙ্গুয়েজে গঠিত ডেটা নিয়ে কাজ করা, যা ডেটা প্রক্রিয়া ও অ্যাক্সেস করা জড়িত। প্রোগ্রামাররা অন্যান্য সিস্টেমের সাথে ইন্টারঅপারিবিলিটি সম্ভব করে তুলতে অথবা কনফিগারেশন ফাইল, ডেটা ফিড এবং ওয়েব সার্ভিসে প্রচলিত অন্যান্য গঠনযুক্ত ডকুমেন্ট পড়া ও লিখার জন্য XML এবং কাজ করে থাকেন।

## কিভাবে:
```PowerShell
# একটি XML ফাইল একটি ভেরিয়েবলে লোড করা
[xml]$xmlContent = Get-Content 'path\to\your\file.xml'

# XML নোড অ্যাক্সেস করা
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "শিরোনাম: $($book.title)"
}

# একটি নতুন XML এলিমেন্ট তৈরি করা
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# XML ফাইলটি আবার সংরক্ষণ করা
$xmlContent.Save('path\to\your\updated\file.xml')
```
নমুনা আউটপুট:
```
শিরোনাম: Programming PowerShell
শিরোনাম: XML Essentials
```

## গভীর ডুব
XML, বা ইএক্সটেনসিবল মার্কআপ ল্যাঙ্গুয়েজ, ৯০ এর দশকের শেষ থেকে রয়েছে এবং এটি গঠনমূলক ডেটার জন্য একটি বিস্তৃতভাবে ব্যবহৃত ফর্ম্যাট হিসেবে অব্যাহত রয়েছে। PowerShell পরম্পরাগত পার্সিং পদ্ধতির তুলনায় XML এর সাথে কাজ করা সরল করে, এটি সরাসরি XML কে অবজেক্টে রূপান্তরিত করে, এতে আপনি পরিচিত ডট নোটেশন এর মাধ্যমে উপাদান নিয়ে ইন্টার্যাক্ট করতে পারেন।

XML এর বিকল্পগুলির মধ্যে JSON, YAML, অথবা কাস্টম ডেটা ফর্ম্যাট অন্তর্ভুক্ত। যেমন, JSON ওয়েব প্রযুক্তিগুলির সাথে এর হালকা প্রকৃতি ও ব্যবহারের সহজতার জন্য জনপ্রিয়তা পেয়েছে। তবে, নামস্থান, স্কিমা, এবং XSLT প্রসেসিং এর মতো XML এর বর্ধিত বৈশিষ্ট্যগুলি অনেক সময়ে জটিল ডকুমেন্ট অথবা শিল্প মানদণ্ডের জন্য এটিকে একটি ভালো পছন্দ করে তোলে।

PowerShell .NET ফ্রেমওয়ার্কের XML ক্ষমতার ব্যবহার করে এর XML হ্যান্ডলিং প্রক্রিয়া চালনা করে। এই মানে হল এটা শুধুমাত্র সিম্পল রিড-রাইট অপারেশন নয়; আপনি স্কিমা ব্যবহার করে ভ্যালিডেশনের জন্য, XPath ব্যবহার করে ক্যোয়েরিস করার জন্য, এবং XSLT ট্রান্সফর্মেশন সম্পাদনের জন্য, সবকিছু যা PowerShell এর মাধ্যমে করা যায়।

## আরো দেখুন
- [W3Schools XML টিউটোরিয়াল](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/json-en.html)
