---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:17.187311-06:00
description: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\
  \u09CD\u09B8\u099F\u09CD\u09B0\u09CD\u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\
  \u09CD\u099F \u09AE\u09BE\u09A8\u09A6\u09A3\u09CD\u09A1 \u0985\u09A8\u09C1\u09AF\
  \u09BE\u09AF\u09BC\u09C0 \u099B\u09CB\u099F \u099B\u09CB\u099F \u0985\u0982\u09B6\
  \ \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u09A1\u09BE\u099F\u09BE \u09A8\u09BF\u09AF\u09BC\u09A8\u09CD\u09A4\u09CD\u09B0\
  \u09A3 \u098F\u09AC\u0982 \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3\u09C7\
  \u09B0\u2026"
lastmod: '2024-03-17T18:47:44.259960-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\
  \u09CD\u09B8\u099F\u09CD\u09B0\u09CD\u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\
  \u09CD\u099F \u09AE\u09BE\u09A8\u09A6\u09A3\u09CD\u09A1 \u0985\u09A8\u09C1\u09AF\
  \u09BE\u09AF\u09BC\u09C0 \u099B\u09CB\u099F \u099B\u09CB\u099F \u0985\u0982\u09B6\
  \ \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u09A1\u09BE\u099F\u09BE \u09A8\u09BF\u09AF\u09BC\u09A8\u09CD\u09A4\u09CD\u09B0\
  \u09A3 \u098F\u09AC\u0982 \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3\u09C7\
  \u09B0\u2026"
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
সাবস্ট্রিং এক্সট্র্যাক্ট করা মানে একটি স্ট্রিং থেকে নির্দিষ্ট মানদণ্ড অনুযায়ী ছোট ছোট অংশ বের করা। প্রোগ্রামাররা টেক্সট ডাটা নিয়ন্ত্রণ এবং বিশ্লেষণের জন্য সাবস্ট্রিং এক্সট্র্যাক্ট করে, যেমন ইনপুটকে আরও উপযোগী অংশে কেটে নেওয়া অথবা একটি বাক্যের অভ্যন্তরে লুকানো তথ্য পেতে।

## কিভাবে:
পাওয়ারশেলে স্ট্রিংগুলি কিভাবে কাটা যায় তা এখানে দেখানো হয়েছে:

```PowerShell
# একটি স্ট্রিং দেওয়া আছে
$text = "Power up your PowerShell skills!"

# সাবস্ট্রিং মেথড ব্যবহার করে এক্সট্র্যাক্ট করা
$startIndex = 10
$length = 9
$substring = $text.Substring($startIndex, $length)
Write-Host $substring  # আউটপুট: your Powe

# রেঞ্জ অপারেটর ব্যবহার করে এক্সট্র্যাক্ট করা
$subrange = $text[10..18] -join ''
Write-Host $subrange  # আউটপুট: your Powe

# শুরু থেকে কোনও নির্দিষ্ট স্থান পর্যন্ত এক্সট্র্যাক্ট করা
$firstPart = $text.Substring(0, $startIndex)
Write-Host $firstPart  # আউটপুট: Power up 

# নির্দিষ্ট ক্যারেক্টারের পর থেকে এক্সট্র্যাক্ট করা
$splitString = $text.Split(" ")[2]
Write-Host $splitString  # আউটপুট: your
```

## গভীর ডাইভ
অনেক আগে, PowerShell মাত্র বেসিক স্ট্রিং মেথডগুলি সাপোর্ট করত। এখন, এটি একটি আলাদা খেলা। `.Substring()` মেথডটি চালু রয়েছে এবং বেশ সোজা—এটি একটি শুরুর ইনডেক্স এবং একটি ঐচ্ছিক দৈর্ঘ্য চায়, এবং এটি আপনার প্রয়োজনীয় অংশটি কেটে ফেলবে। PowerShell 6 এ শুরু করে, আপনি রেঞ্জ অপারেটর ব্যবহার করতে পারেন, যা বিশেষ করে যখন আপনি পরিবর্তনশীল-দৈর্ঘ্যের স্ট্রিংগুলি সামলাচ্ছেন তখন আরও সহজ হতে পারে।

অনুরূপভাবে, `-split` অপারেটর এবং `.Split()` মেথড দুটোই প্যাটার্ন বা ক্যারেক্টারগুলির উপর ভিত্তি করে স্ট্রিংগুলি কাটার জন্য দারুন কাজে লাগে। নির্দিষ্ট একটি খণ্ড প্রয়োজন? এই সরঞ্জামগুলি ব্যবহার করুন।

কর্মক্ষমতার দিক থেকে, ছোট কাজের জন্য খুব বেশি পার্থক্য নেই। যখন আপনি বিশাল টেক্সট ফাইলগুলির সাথে কাজ করছেন অথবা প্রতি মিলিসেকেন্ডে লুপ করছেন, তখন আপনার বেঞ্চমার্ক চাইবেন। অন্যথায়, এটি আপনার স্ক্রিপ্টের জন্য যা আরও পঠনযোগ্য এবং ঠিক মনে হয় সেই সম্পর্কে বেশি।

মনে রাখবেন, PowerShell স্ট্রিংগুলি শূন্য থেকে ইনডেক্সড হয়—অনেক প্রোগ্রামিং ভাষায় সাধারণ। এক উপাদান কম ভুলের জন্য সাবধান থাকুন।

## আরও দেখুন
PowerShell-এ স্ট্রিং ম্যানিপুলেশন সম্পর্কে আরও জানতে:

- [About_Split](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_split?view=powershell-7)
- [About Comparison Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7) যা -split নিয়ে আলোচনা করে
