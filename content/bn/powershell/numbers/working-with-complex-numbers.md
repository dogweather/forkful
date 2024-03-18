---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:38.802005-06:00
description: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE, \u09AF\
  \u09C7\u0997\u09C1\u09B2\u09CB\u09B0 \u098F\u0995\u099F\u09BF \u09AC\u09BE\u09B8\
  \u09CD\u09A4\u09AC \u0985\u0982\u09B6 \u098F\u09AC\u0982 \u098F\u0995\u099F\u09BF\
  \ \u0995\u09BE\u09B2\u09CD\u09AA\u09A8\u09BF\u0995 \u0985\u0982\u09B6 \u09A5\u09BE\
  \u0995\u09C7 (\u09AF\u09C7\u09AE\u09A8 \u09E9 + \u09EAi), \u0987\u099E\u09CD\u099C\
  \u09BF\u09A8\u09BF\u09AF\u09BC\u09BE\u09B0\u09BF\u0982, \u09AA\u09A6\u09BE\u09B0\
  \u09CD\u09A5\u09AC\u09BF\u099C\u09CD\u099E\u09BE\u09A8, \u098F\u09AC\u0982 \u09A1\
  \u09C7\u099F\u09BE \u09AC\u09BF\u099C\u09CD\u099E\u09BE\u09A8\u09C7 \u0985\u09AA\
  \u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.264953-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE, \u09AF\u09C7\
  \u0997\u09C1\u09B2\u09CB\u09B0 \u098F\u0995\u099F\u09BF \u09AC\u09BE\u09B8\u09CD\
  \u09A4\u09AC \u0985\u0982\u09B6 \u098F\u09AC\u0982 \u098F\u0995\u099F\u09BF \u0995\
  \u09BE\u09B2\u09CD\u09AA\u09A8\u09BF\u0995 \u0985\u0982\u09B6 \u09A5\u09BE\u0995\
  \u09C7 (\u09AF\u09C7\u09AE\u09A8 \u09E9 + \u09EAi), \u0987\u099E\u09CD\u099C\u09BF\
  \u09A8\u09BF\u09AF\u09BC\u09BE\u09B0\u09BF\u0982, \u09AA\u09A6\u09BE\u09B0\u09CD\
  \u09A5\u09AC\u09BF\u099C\u09CD\u099E\u09BE\u09A8, \u098F\u09AC\u0982 \u09A1\u09C7\
  \u099F\u09BE \u09AC\u09BF\u099C\u09CD\u099E\u09BE\u09A8\u09C7 \u0985\u09AA\u09B0\
  \u09BF\u09B9\u09BE\u09B0\u09CD\u09AF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?
জটিল সংখ্যা, যেগুলোর একটি বাস্তব অংশ এবং একটি কাল্পনিক অংশ থাকে (যেমন ৩ + ৪i), ইঞ্জিনিয়ারিং, পদার্থবিজ্ঞান, এবং ডেটা বিজ্ঞানে অপরিহার্য। প্রোগ্রামাররা তাদের সিমুলেশন, সিগন্যাল প্রসেসিং, এবং বিশেষ ধরনের গাণিতিক সমস্যা সমাধানের জন্য ব্যবহার করে থাকেন।

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
