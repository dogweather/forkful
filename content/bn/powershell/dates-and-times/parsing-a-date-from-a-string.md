---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:06:23.995746-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PowerShell \u098F\u09B0 `Get-Date`\
  \ \u0995\u09AE\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09B2\u09C7\u099F \u098F\u09AC\
  \u0982 `[datetime]` \u099F\u09BE\u0987\u09AA \u0985\u09CD\u09AF\u09BE\u0995\u09CD\
  \u09B8\u09BF\u09B2\u09BE\u09B0\u09C7\u099F\u09B0 \u098F\u09B0 \u09B8\u09BE\u09B9\
  \u09BE\u09AF\u09CD\u09AF\u09C7 \u09B8\u09C1\u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\
  \u09CD\u099F \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09AC\u09BF\u09A8\u09CD\
  \u09AF\u09BE\u09B8\u0997\u09C1\u09B2\u09BF \u09A5\u09C7\u0995\u09C7 \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\
  \u0996\u2026"
lastmod: '2024-04-05T21:53:52.797312-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u098F\u09B0 `Get-Date` \u0995\u09AE\u09CD\u09AF\u09BE\u09A8\u09CD\
  \u09A1\u09B2\u09C7\u099F \u098F\u09AC\u0982 `[datetime]` \u099F\u09BE\u0987\u09AA\
  \ \u0985\u09CD\u09AF\u09BE\u0995\u09CD\u09B8\u09BF\u09B2\u09BE\u09B0\u09C7\u099F\
  \u09B0 \u098F\u09B0 \u09B8\u09BE\u09B9\u09BE\u09AF\u09CD\u09AF\u09C7 \u09B8\u09C1\
  \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A4\u09BE\u09B0\u09BF\u0996\
  \u09C7\u09B0 \u09AC\u09BF\u09A8\u09CD\u09AF\u09BE\u09B8\u0997\u09C1\u09B2\u09BF\
  \ \u09A5\u09C7\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\
  \u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\
  \u09B0\u09BE \u0996\u09C1\u09AC\u0987 \u09B8\u09C7\u09BE\u099C\u09BE\u0964 \u0985\
  \u09A7\u09BF\u0995 \u099C\u099F\u09BF\u09B2 \u09AC\u09BE \u0985-\u09AE\u09BE\u09A8\
  \u0995 \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982\u0997\u09C1\u09B2\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF, `[datetime]::ParseExact`\
  \ \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u099F\u09BF \u09AA\u09CD\u09B0\u09AF\u09BC\
  \u09CB\u0997 \u0995\u09B0\u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\
  \ \u09AF\u09BE\u09A4\u09C7 \u09B8\u09A0\u09BF\u0995 \u09AC\u09BF\u09A8\u09CD\u09AF\
  \u09BE\u09B8 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0995\u09B0\
  \u09BE \u09AF\u09BE\u09AF\u09BC\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কিভাবে:
PowerShell এর `Get-Date` কম্যান্ডলেট এবং `[datetime]` টাইপ অ্যাক্সিলারেটর এর সাহায্যে সুনির্দিষ্ট তারিখের বিন্যাসগুলি থেকে স্ট্রিং থেকে তারিখ পার্স করা খুবই সোজা। অধিক জটিল বা অ-মানক তারিখের স্ট্রিংগুলির জন্য, `[datetime]::ParseExact` পদ্ধতিটি প্রয়োগ করা যেতে পারে যাতে সঠিক বিন্যাস নির্দিষ্ট করা যায়।

### `Get-Date` এবং `[datetime]` ব্যবহার করে:
```powershell
# Get-Date ব্যবহার করে সহজ রূপান্তর
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**নমুনা আউটপুট:**
```
Saturday, April 1, 2023 12:00:00 AM
```

```powershell
# টাইপ অ্যাক্সিলারেটর [datetime] ব্যবহার করা
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**নমুনা আউটপুট:**
```
Saturday, April 1, 2023 12:00:00 AM
```

### অ-মানক বিন্যাসের জন্য `[datetime]::ParseExact` ব্যবহার:
স্বয়ংক্রিয়ভাবে সনাক্ত করা না যাওয়া বিন্যাসের জন্য, আপনি সঠিক রূপান্তর নিশ্চিত করার জন্য সঠিক বিন্যাসটি নির্দিষ্ট করতে পারেন।
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**নমুনা আউটপুট:**
```
Saturday, April 1, 2023 2:00:00 PM
```

### তৃতীয়-পক্ষের লাইব্রেরিসমূহ ব্যবহার:
যদিও PowerShell নিজেই তারিখ পার্সিংয়ের জন্য বেশ শক্তিশালী, খুব জটিল কিছু পরিস্থিতি বা অতিরিক্ত কার্যকারিতার জন্য, আপনি .NET লাইব্রেরিগুলি যেমন NodaTime ব্যবহার করতে পারেন, যদিও অনেক সাধারণ ব্যবহারের ক্ষেত্রে, PowerShell-এর নিজস্ব ক্ষমতা যথেষ্ট হতে পারে।

```powershell
# শুধুমাত্র একটি উদাহরণ হিসেবে NodaTime ব্যবহার, মনে রাখবেন আপনার প্রকল্পে লাইব্রেরিটি যোগ করা প্রয়োজন
# Install-Package NodaTime -Version 3.0.5
# NodaTime ব্যবহার করে একটি তারিখ পার্স করা
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**নোট নমুনা:** উপরের কোডটি একটি ধারণাগত উদাহরণ। বাস্তব অনুশীলনে, NodaTime আপনার প্রকল্পে সঠিকভাবে যুক্ত হয়েছে কিনা তা নিশ্চিত করা নিশ্চিত করুন, যাতে প্রকার এবং পদ্ধতিগুলি উপলব্ধ হয়।
