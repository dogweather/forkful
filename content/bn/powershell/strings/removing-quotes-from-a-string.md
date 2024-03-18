---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:16:15.424705-06:00
description: "PowerShell \u098F \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\
  \ \u099A\u09BF\u09B9\u09CD\u09A8 \u0985\u09AA\u09B8\u09BE\u09B0\u09A3 \u0986\u09AA\
  \u09A8\u09BE\u09B0 \u099F\u09C7\u0995\u09CD\u09B8\u099F\u09C7\u09B0 \u099A\u09BE\
  \u09B0\u09AA\u09BE\u09B6\u09C7 \u09B0\u09BE\u0996\u09BE \u098F\u0995\u0995 (`'`)\
  \ \u09AC\u09BE \u09A6\u09CD\u09AC\u09C8\u09A4 (`\"`) \u0989\u09A6\u09CD\u09A7\u09C3\
  \u09A4\u09BF \u099A\u09BF\u09B9\u09CD\u09A8\u0997\u09C1\u09B2\u09BF \u09B8\u09B0\
  \u09BF\u09AF\u09BC\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.258765-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u098F \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF\
  \ \u099A\u09BF\u09B9\u09CD\u09A8 \u0985\u09AA\u09B8\u09BE\u09B0\u09A3 \u0986\u09AA\
  \u09A8\u09BE\u09B0 \u099F\u09C7\u0995\u09CD\u09B8\u099F\u09C7\u09B0 \u099A\u09BE\
  \u09B0\u09AA\u09BE\u09B6\u09C7 \u09B0\u09BE\u0996\u09BE \u098F\u0995\u0995 (`'`)\
  \ \u09AC\u09BE \u09A6\u09CD\u09AC\u09C8\u09A4 (`\"`) \u0989\u09A6\u09CD\u09A7\u09C3\
  \u09A4\u09BF \u099A\u09BF\u09B9\u09CD\u09A8\u0997\u09C1\u09B2\u09BF \u09B8\u09B0\
  \u09BF\u09AF\u09BC\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\
  \u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE"
---

{{< edit_this_page >}}

## কী এবং কেন?
PowerShell এ একটি স্ট্রিং থেকে উদ্ধৃতি চিহ্ন অপসারণ আপনার টেক্সটের চারপাশে রাখা একক (`'`) বা দ্বৈত (`"`) উদ্ধৃতি চিহ্নগুলি সরিয়ে দেয়। প্রোগ্রামাররা প্রায়শই প্রক্রিয়াজাতি, তুলনা, অথবা আউটপুট উদ্দেশ্যে স্ট্রিংগুলোকে পরিষ্কার করতে প্রয়োজন হয়, বিশেষত যখন ব্যবহারকারীর ইনপুট বা ফাইল পার্সিং নিয়ে কাজ করা হয়।

## কীভাবে:
আপনি `-replace` অপারেটর ব্যবহার করে স্ট্রিং থেকে উদ্ধৃতি চিহ্নগুলি ফেলে দিতে পারেন। এখানে এর ব্যবহার দেওয়া হল:

```PowerShell
# একক উদ্ধৃতি প্রতিস্থাপন
$stringWithSingleQuotes = "'Hello, World!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # আউটপুট: Hello, World!

# দ্বৈত উদ্ধৃতি প্রতিস্থাপন
$stringWithDoubleQuotes = '"Hello, World!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # আউটপুট: Hello, World!
```

উভয় ধরনের জন্য:

```PowerShell
$stringWithQuotes = '"Hi there," she said.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # এখানে regex ক্যারেক্টার ক্লাসের ব্যবহার লক্ষ্য করুন
Write-Output $cleanString  # আউটপুট: Hi there, she said.
```

কনসোল থেকে নমুনা আউটপুট এরকম হবে:

```
Hello, World!
Hello, World!
Hi there, she said.
```

## গভীরে ডুব:
মাইক্রোসফটের চোখে PowerShell একটি চিন্তা হওয়ার আগে, উইন্ডোজে টেক্সট প্রসেসিং প্রায়শই ব্যাচ স্ক্রিপ্টগুলোর অধীন ছিল যা সীমিত ক্ষমতা প্রদান করতো। PowerShell এর প্রবর্তনের সাথে এটি শক্তিশালী স্ট্রিং ম্যানিপুলেশন সুবিধাগুলি নিয়ে এসেছে যা স্ক্রিপ্টিংয়ে অনেক বেশি সক্ষম করে তুলেছে।

`-replace`-এর বিকল্প বিদ্যমান আছে, যেমন স্ট্রিং এর শুরু এবং শেষের উদ্ধৃতি চিহ্ন সরাতে `.Trim()` পদ্ধতি ব্যবহার করা, কিন্তু এগুলি একই নিয়ন্ত্রণ বা regex সাপোর্ট অফার করে না।

```PowerShell
# শুরু এবং শেষের উদ্ধৃতির জন্য .Trim() ব্যবহার
$stringWithQuotes = '"Hello, World!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # আউটপুট: Hello, World!
```

খেয়াল রাখবেন, `-replace` এর পেছনে regex ব্যবহার করে, তাই এর সাথে কাজ করার সময় মনে রাখবেন যে বিশেষ চরিত্রগুলি টার্গেট করার জন্য এস্কেপ করা প্রয়োজন। যদি আপনি উদ্ধৃতি অপসারণের উপর আরো সূক্ষ্ম নিয়ন্ত্রণ চান, `-replace` সাহায্যে regex এ ডুব দিয়ে এই পদ্ধতিতে যাওয়া আপনাকে অপরিমেয় লচছিলতা দেয়।

## আরো দেখুন
- PowerShell-এ রেগেক্স সম্পর্কে আরো জানতে, অফিশিয়াল ডক্স দেখুন: [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- অন্যান্য স্ট্রিং পদ্ধতিগুলি আবিষ্কার করুন: [Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/en-us/dotnet/api/system.string.trim?view=net-6.0)
