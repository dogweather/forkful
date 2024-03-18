---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:07:46.726221-06:00
description: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F\
  \ \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE \u09AF\u09C7\u09A8\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 \u098F\u0995\u099F\u09BF \u0995\u09A5\u09CB\u09AA\u0995\u09A5\u09A8 \u0995\
  \u09B0\u09BE\u0964 \u098F\u099F\u09BF \u09AE\u09C2\u09B2\u09A4 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u0985\u09A7\u09C0\u09A8\u09B8\u09CD\
  \u09A5 \u0995\u09BF \u0998\u099F\u099B\u09C7 \u09A4\u09BE \u09A6\u09C7\u0996\u09BE\
  \u09A8\u09CB\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\
  \u099F \u09B8\u09CD\u099F\u09C7\u099F\u09AE\u09C7\u09A8\u09CD\u099F \u09B8\u0982\
  \u09AF\u09CB\u099C\u09A8 \u0995\u09B0\u09BE\u09B0 \u09AC\u09BF\u09B7\u09AF\u09BC\
  \u0964\u2026"
lastmod: '2024-03-17T18:47:44.278904-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE \u09AF\u09C7\u09A8 \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u098F\u0995\u099F\u09BF \u0995\u09A5\u09CB\u09AA\u0995\u09A5\u09A8 \u0995\u09B0\
  \u09BE\u0964 \u098F\u099F\u09BF \u09AE\u09C2\u09B2\u09A4 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u0985\u09A7\u09C0\u09A8\u09B8\u09CD\u09A5\
  \ \u0995\u09BF \u0998\u099F\u099B\u09C7 \u09A4\u09BE \u09A6\u09C7\u0996\u09BE\u09A8\
  \u09CB\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F\
  \ \u09B8\u09CD\u099F\u09C7\u099F\u09AE\u09C7\u09A8\u09CD\u099F \u09B8\u0982\u09AF\
  \u09CB\u099C\u09A8 \u0995\u09B0\u09BE\u09B0 \u09AC\u09BF\u09B7\u09AF\u09BC\u0964\
  \u2026"
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

ডিবাগ আউটপুট প্রিন্ট করা যেন আপনার কোডের সাথে একটি কথোপকথন করা। এটি মূলত প্রোগ্রামের অধীনস্থ কি ঘটছে তা দেখানোর জন্য প্রিন্ট স্টেটমেন্ট সংযোজন করার বিষয়। প্রোগ্রামাররা ভেরিয়েবলগুলি, নির্বাহের প্রবাহ এবং কেন জিনিসগুলি ভুল হতে পারে তা বুঝতে এটি করে থাকেন।

## কিভাবে:

চলুন এটিকে সহজ রাখি এবং প্রকৃতপক্ষে কিছু করি। আমরা একটি ভেরিয়েবলের মান, একটি লুপ কেমন এগোচ্ছে এবং যে বিরক্তিকর ত্রুটি দেখা দিতে পারে তা ধরে ফেলার প্রদর্শন করব।

```PowerShell
# একটি ভেরিয়েবলের মান দেখানো
$name = "PowerShell Guru"
Write-Host "The value of name is: $name"

# একটি লুপের অগ্রগতি মনিটর করা
for ($i = 0; $i -lt 5; $i++) {
    Write-Host "We're on loop number: $i"
}

# ত্রুটি ধরা এবং প্রিন্ট করা
try {
    Get-Item "C:\NonExistent\File.txt" -ErrorAction Stop
} catch {
    Write-Host "Oops: $_"
}
```

নমুনা আউটপুট:

```
The value of name is: PowerShell Guru
We're on loop number: 0
We're on loop number: 1
We're on loop number: 2
We're on loop number: 3
We're on loop number: 4
Oops: C:\NonExistent\File.txt' পথ পাওয়া যায়নি কারণ এটি অস্তিত্ব বিদ্যমান নেই।
```

## গভীর ডুব

কম্পিউটিংয়ের প্রাচীন যুগে, ডিবাগিং প্রায়ই শাব্দিক ভৌতিক বাগ দ্বারা হার্ডওয়্যারের সাথে ঝামেলা থাকার মানে ছিল। এর থেকে আমরা বেশ দূর এগিয়ে এসেছি, এখন "বাগ" শব্দটি কোড সমস্যার জন্য, এবং "ডিবাগিং" শব্দটি তাদের সমাধানের জন্য ব্যবহার করা হয়।

`Write-Host` cmdlet পাওয়ারশেলের প্রিন্ট করার জন্য একটি বন্ধু, যা মৌলিক স্ক্রিপ্টের জন্য ভালো, কিন্তু আরও চমৎকার উপায় আছে: `Write-Verbose`, `Write-Debug`, `Write-Output`, এবং `Write-Information` বিভিন্ন প্রকারের আউটপুটের জন্য বিভিন্ন স্বাদের মত। এগুলি যখন আপনার স্ক্রিপ্টকে নিশ্চুপ করতে হবে অথবা কনসোলে স্প্যাম ছাড়া জিনিস লগ করা দরকার তখন নিয়ন্ত্রিত বাগ্বিধান দেয়।

বাস্তবায়নের দিক থেকে, পাওয়ারশেলের ত্রুটি হ্যান্ডলিং বিশেষ চমৎকার। আপনি `try`, `catch`, `finally` ব্লক দ্বারা বিভিন্ন ধরনের ব্যতিক্রম ধরতে পারেন এবং কিভাবে প্রতিক্রিয়া জানাবেন তা সিদ্ধান্ত নিতে পারেন। এটি যেন ত্রুটি ব্যবস্থাপনার জন্য আপনার-নিজের-অভিযান নির্বাচন করা।

## আরও দেখুন

- [ট্রাই, ক্যাচ, ফাইনালি সম্পর্কে](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-exceptions?view=powershell-7.1)
