---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:08.608545-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PowerShell \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC \u09AA\u09C7\u09A4\u09C7 \u09B8\
  \u09CB\u099C\u09BE \u0995\u09CD\u09AF\u09BE\u09AE\u09CD\u09AA\u09B2\u09C7\u099F\
  \ \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 `Get-Date` \u0995\
  \u09CD\u09AF\u09BE\u09AE\u09CD\u09AA\u09B2\u09C7\u099F\u099F\u09BF \u098F\u0987\
  \ \u0989\u09A6\u09CD\u09A6\u09C7\u09B6\u09CD\u09AF\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09AA\u09CD\u09B0\u09BE\u09A5\u09AE\u09BF\u0995 \u099F\u09C1\u09B2\u0964\
  \ \u098F\u099F\u09BF \u09AA\u09C2\u09B0\u09CD\u09A3 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC\u2026"
lastmod: '2024-03-17T18:47:44.287847-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\
  \u09AF\u09BC \u09AA\u09C7\u09A4\u09C7 \u09B8\u09CB\u099C\u09BE \u0995\u09CD\u09AF\
  \u09BE\u09AE\u09CD\u09AA\u09B2\u09C7\u099F \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8\
  \ \u0995\u09B0\u09C7\u0964 `Get-Date` \u0995\u09CD\u09AF\u09BE\u09AE\u09CD\u09AA\
  \u09B2\u09C7\u099F\u099F\u09BF \u098F\u0987 \u0989\u09A6\u09CD\u09A6\u09C7\u09B6\
  \u09CD\u09AF\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09BE\u09A5\
  \u09AE\u09BF\u0995 \u099F\u09C1\u09B2\u0964 \u098F\u099F\u09BF \u09AA\u09C2\u09B0\
  \u09CD\u09A3 \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\
  \u09BC \u09B0\u09BF\u099F\u09BE\u09B0\u09CD\u09A8 \u0995\u09B0\u09C7, \u09AF\u09BE\
  \ \u0986\u09AA\u09A8\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u09AF\
  \u09BC\u09CB\u099C\u09A8 \u09AE\u09A4\u09CB \u09AB\u09B0\u09AE\u09C7\u099F \u0985\
  \u09A5\u09AC\u09BE \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964."
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কিভাবে:
PowerShell তারিখ এবং সময় পেতে সোজা ক্যাম্পলেট প্রদান করে। `Get-Date` ক্যাম্পলেটটি এই উদ্দেশ্যের জন্য প্রাথমিক টুল। এটি পূর্ণ তারিখ এবং সময় রিটার্ন করে, যা আপনি আপনার প্রয়োজন মতো ফরমেট অথবা পরিবর্তন করতে পারেন।

```powershell
# বর্তমানের তারিখ এবং সময় পেতে
Get-Date
```

**উদাহরণের আউটপুট:**

```
Tuesday, September 5, 2023 9:46:02 AM
```

আপনি আউটপুটটি কেবল আপনার দরকারি তথ্যটুকু দেখাতে ফরমেট করতে পারেন, যেমন কেবল তারিখ অথবা কেবল সময়।

```powershell
# নির্দিষ্ট ফরম্যাটে কেবল বর্তমানের তারিখ
Get-Date -Format "yyyy-MM-dd"
```

**উদাহরণের আউটপুট:**

```
2023-09-05
```

```powershell
# কেবল বর্তমানের সময়
Get-Date -Format "HH:mm:ss"
```

**উদাহরণের আউটপুট:**

```
09:46:02
```

### .NET ক্লাস ব্যবহার করে
PowerShell সরাসরি ডট নেট ক্লাসে অ্যাক্সেস দেয়, যা তারিখ এবং সময়ের সাথে কাজ করার বিকল্প উপায় অফার করে।

```powershell
# ডট নেট DateTime ক্লাস ব্যবহার করে বর্তমানের তারিখ এবং সময় পেতে
[System.DateTime]::Now
```

**উদাহরণের আউটপুট:**

```
Tuesday, September 5, 2023 9:46:02 AM
```

UTC সময়ের জন্য:

```powershell
# ডট নেট DateTime ক্লাস ব্যবহার করে বর্তমানের UTC তারিখ এবং সময় পেতে
[System.DateTime]::UtcNow
```

**উদাহরণের আউটপুট:**

```
Tuesday, September 5, 2023 1:46:02 PM
```

এই কম্যান্ড এবং ক্লাস গুলো PowerShell এ তারিখ এবং সময়ের সাথে কাজ করার জন্য শক্তিশালী এবং নমনীয় অপশন প্রদান করে, যা অনেক স্ক্রিপ্টিং এবং অটোমেশন কাজের জন্য অত্যাবশ্যক।
