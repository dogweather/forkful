---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:48.018962-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u099B\u09CB\u099F \u0985\u0995\u09CD\u09B7\u09B0\u09C7 \u09AA\u09B0\u09BF\
  \u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE\u09B0 \u09AE\u09BE\u09A8\u09C7\
  \ \u09B9\u09B2 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u099F\u09BF\u09B0 \u09AA\
  \u09CD\u09B0\u09A4\u09CD\u09AF\u09C7\u0995\u099F\u09BF \u0985\u0995\u09CD\u09B7\u09B0\
  \u0995\u09C7 \u099B\u09CB\u099F \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\
  \u09B7\u09B0\u09C7 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\
  \u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u099F\u09C7\u0995\u09CD\u09B8\u099F\u0995\u09C7 \u09AE\u09BE\u09A8\u0995\
  \u09BF\u0995\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF, \u09A4\u09C1\u09B2\
  \u09A8\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.257763-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u099B\u09CB\u099F \u0985\u0995\u09CD\u09B7\u09B0\u09C7 \u09AA\u09B0\u09BF\
  \u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE\u09B0 \u09AE\u09BE\u09A8\u09C7\
  \ \u09B9\u09B2 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u099F\u09BF\u09B0 \u09AA\
  \u09CD\u09B0\u09A4\u09CD\u09AF\u09C7\u0995\u099F\u09BF \u0985\u0995\u09CD\u09B7\u09B0\
  \u0995\u09C7 \u099B\u09CB\u099F \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\
  \u09B7\u09B0\u09C7 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\
  \u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u099F\u09C7\u0995\u09CD\u09B8\u099F\u0995\u09C7 \u09AE\u09BE\u09A8\u0995\
  \u09BF\u0995\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF, \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u098F\u09AC\u0982\
  \ \u0995\u0996\u09A8\u0993 \u0995\u0996\u09A8\u0993 \u0995\u09CB\u09A1\u09BF\u0982\
  \ \u09AC\u09BE \u09A1\u09BE\u099F\u09BE \u09B8\u09CD\u099F\u09CB\u09B0\u09C7\u099C\
  \u09C7 \u0995\u09C7\u09B8-\u09B8\u09C7\u09A8\u09B8\u09BF\u099F\u09BF\u09AD \u09A8\
  \u09BF\u09DF\u09AE \u0985\u09A8\u09C1\u09B8\u09B0\u09A3 \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF \u0995\u09B0\u09C7\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কিভাবে:
PowerShell স্ট্রিংগুলির সাথে বেশ কার্যকরী। `.ToLower()` মেথড ব্যবহার করুন, এরকম:

```PowerShell
$string = "HELLO, World!"
$lowerCaseString = $string.ToLower()
$lowerCaseString
```

আউটপুট:

```
hello, world!
```

যখন সাংস্কৃতিক নরমের প্রভাব পরিবর্তনে এড়িয়ে চান তখন `ToLowerInvariant()` মেথড চেষ্টা করুন:

```PowerShell
$string = "HELLO, World!"
$lowerCaseInvariant = $string.ToLowerInvariant()
$lowerCaseInvariant
```

আউটপুট:

```
hello, world!
```

## গভীরে ডুব দেওয়া
একসময়, কেস অ-সেন্সিটিভিটি প্রোগ্রামিং ভাষাগুলিতে বেশ সাধারণ ছিল। PowerShell এ, এর .NET পূর্বপুরুষদের মত, স্ট্রিংগুলি অবজেক্ট যা নিপুণভাবে ম্যানিপুলেশনের জন্য অন্তর্নির্মিত মেথড সহ আসে। যখন আমরা `.ToLower()` ব্যবহার করি, আমরা একটি মেথড আহ্বান করি যা আমাদের জন্য পরিবর্তন প্রক্রিয়াটি সম্পন্ন করে।

কাজটি সম্পন্ন করার অন্যান্য উপায়? অবশ্যই। আপনি ব্যবহার করতে পারেন:

- প্রতিটি অক্ষর পরিদর্শন করার জন্য একটি `for` লুপ, এবং ম্যানুয়ালি কেস পরিবর্তন
- রেগুলার এক্সপ্রেশনস ব্যবহার করে `-replace` অপারেটর
- `.ToLower()` এর বিভিন্ন ওভারলোড ব্যবহার করে সংস্কৃতি-নির্দিষ্ট রূপান্তর

`ToLowerInvariant()` সহ অপরিবর্তিত সংস্কৃতি ব্যবহার করার কারণ কি? ভিন্ন লোকেলগুলিতে যেখানে "লোয়ার" কেসের ব্যাখ্যা পার্থক্য হতে পারে, সেখানে সম্মতি ফলাফলের জন্য এটা অপরিহার্য।

## আরও দেখুন
স্ট্রিং ম্যানিপুলেশনে আরও বিস্তারিত অভিযানের জন্য, এই লিংকগুলি দেখুন:

- [.NET স্ট্রিং ক্লাস](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-6.0)
