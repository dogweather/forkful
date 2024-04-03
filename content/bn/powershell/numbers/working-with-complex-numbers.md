---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:38.802005-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PowerShell-\u098F \u099C\u099F\
  \u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09B8\u09AE\u09B0\u09CD\u09A5\
  \u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\
  \u09A4 \u0995\u09CB\u09A8 \u0985\u09AA\u09B6\u09A8 \u09A8\u09C7\u0987, \u09A4\u09BE\
  \u0987 \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09B9\u09AF\u09BC \u09A8\u09BF\u099C\
  \u09C7\u09B0 \u09B8\u09AE\u09BE\u09A7\u09BE\u09A8 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7 \u0985\u09A5\u09AC\u09BE .NET-\u098F\u09B0\
  \ `System.Numerics.Complex`\u2026"
lastmod: '2024-03-17T18:47:44.264953-06:00'
model: gpt-4-0125-preview
summary: "PowerShell-\u098F \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\
  \u09BE \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u0995\u09CB\u09A8 \u0985\u09AA\u09B6\
  \u09A8 \u09A8\u09C7\u0987, \u09A4\u09BE\u0987 \u0986\u09AA\u09A8\u09BE\u0995\u09C7\
  \ \u09B9\u09AF\u09BC \u09A8\u09BF\u099C\u09C7\u09B0 \u09B8\u09AE\u09BE\u09A7\u09BE\
  \u09A8 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7 \u0985\
  \u09A5\u09AC\u09BE .NET-\u098F\u09B0 `System.Numerics.Complex` \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7\u0964."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কিভাবে:
PowerShell-এ জটিল সংখ্যা সমর্থনের জন্য নির্মিত কোন অপশন নেই, তাই আপনাকে হয় নিজের সমাধান তৈরি করতে হবে অথবা .NET-এর `System.Numerics.Complex` ব্যবহার করতে হবে।

```PowerShell
# .NET ব্যবহার করে জটিল সংখ্যা তৈরি করা যাক
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# জটিল সংখ্যা তৈরি করা
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# দুটি জটিল সংখ্যা যোগ করা
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# দুটি জটিল সংখ্যা গুণ করা
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# ফলাফল দেখানো
"Sum: $sum"
"Product: $product"
```
আউটপুট:
```
Sum: (4, 6)
Product: (-5, 10)
```

## গভীর ডুব
জটিল সংখ্যা বিকশিত হয়েছিল ১৬শ শতাব্দীতে, এমন সমীকরণ সমাধানের জন্য যা বাস্তব সংখ্যার অধীনে সমাধান ছিল না। এখন এগুলো আধুনিক গণিতের একটি মূল ভিত্তি হয়ে উঠেছে।

PowerShell এর জটিল সংখ্যা সমর্থনের জন্য .NET-এর উপর নির্ভরতা মানে পারফরম্যান্স দৃঢ়। বিকল্পের মধ্যে তৃতীয়-পক্ষের লাইব্রেরি অথবা অন্যান্য প্রোগ্রামিং ভাষা রয়েছে, যেমন Python, যেখানে জটিল সংখ্যা একটি নেটিভ ডাটা টাইপ।

## দেখুন সাথে:
- [System.Numerics.Complex কাঠামো](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [Python-এ জটিল সংখ্যার অঙ্কনীতি](https://docs.python.org/3/library/cmath.html)
