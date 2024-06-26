---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:23.226415-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PowerShell `-replace` \u0985\u09AA\
  \u09BE\u09B0\u09C7\u099F\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u09AE\u09C7\u09B2\u09C7 \u098F\u09AE\u09A8 \u0985\u0995\
  \u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\
  \u09B2\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u0995\u09BF\u099B\u09C1 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982-\u09AE\u09C7\u09B0\u09BE\u09AE\u09A4\u09C7\u09B0 \u0995\u09BE\u099C\
  ."
lastmod: '2024-03-17T18:47:44.254736-06:00'
model: gpt-4-0125-preview
summary: "PowerShell `-replace` \u0985\u09AA\u09BE\u09B0\u09C7\u099F\u09B0 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09AA\u09CD\u09AF\u09BE\u099F\
  \u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AE\u09C7\u09B2\
  \u09C7 \u098F\u09AE\u09A8 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF\
  \ \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\u09C7\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09BF\u099B\
  \u09C1 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982-\u09AE\u09C7\u09B0\u09BE\u09AE\
  \u09A4\u09C7\u09B0 \u0995\u09BE\u099C."
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কিভাবে:
PowerShell `-replace` অপারেটর ব্যবহার করে প্যাটার্নের সাথে মেলে এমন অক্ষরগুলি মুছে ফেলে। এখানে আপনার জন্য কিছু স্ট্রিং-মেরামতের কাজ:

```PowerShell
# সাধারণ প্রতিস্থাপন: সংখ্যা অপসারণ
$text = 'ABC123'
$cleanText = $text -replace '\d+'
$cleanText  # আউটপুট: ABC

# সাদাসিধে স্থান মুছে দেওয়া
$text = 'Hello World         '
$trimmedText = $text -replace '\s+$'
$trimmedText  # আউটপুট: Hello World

# নির্দিষ্ট অক্ষর নিক্ষেপ
$text = 'uN_w@nt3d-charact3r$'
$cleanedUpText = $text -replace '[-@3$]', ''
$cleanedUpText  # আউটপুট: uNwntd-charactr
```

## গভীর ডুব
PowerShell -replace অপারেটর regex (রেগুলার এক্সপ্রেশন) ব্যবহার করে, যা প্রায় গুপ্ত শিল্প; ১৯৫০ সাল থেকে এটি প্রচলিত এবং প্যাটার্ন ম্যাচিংয়ের জন্য অনেক প্রোগ্রামিং ভাষায় কাজ করে।

`-replace` ছাড়া বিকল্প কি? সাদাসিধে জিনিসের জন্য, সাদাসিধে স্থানের জন্য রয়েছে `.Trim()` পদ্ধতির পরিবার এবং আক্ষরিক প্রতিস্থাপনের জন্য `.Replace()` পদ্ধতি। কিন্তু প্যাটার্ন-ভিত্তিক অপারেশনের জন্য `-replace` অপারেটর আপনার যেতে হবে।

অভ্যন্তরীণভাবে, `-replace` ব্যবহার করলে, PowerShell .NET Framework-এর regex ক্ষমতা পরিবর্তন করে। এটি একটি শক্তিশালী ম্যাচ-এন্ড-স্লাইস অপারেশন যা প্রতি-অক্ষর স্তরে কাজ করে কি থাকবে এবং কি যাবে তা স্থির করে। মনে রাখা জরুরী, regex প্যাটার্ন জটিল হতে পারে এবং জটিল প্যাটার্নের জন্য আরো প্রক্রিয়া শক্তি খরচ করতে পারে, তাই সাবধানে ব্যবহার করুন!

## আরো দেখুন
রেগেক্স র‍্যাবিট হোলে আরও গভীরে নেমে দেখতে, এগুলি দেখুন:
- [PowerShell-এর তুলনা অপারেটরসমূহ সম্পর্কে](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1)
- [PowerShell দিয়ে বিরক্তিকর কাজ অটোমেট করা](https://adamtheautomator.com/powershell-replace/) বাস্তব জগতের অ্যাপ্লিকেশনের জন্য।
