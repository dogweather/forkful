---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:04:55.760901-06:00
description: "PowerShell-\u098F HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 HTML \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F\
  \u0995\u09C7 \u09AA\u09B0\u09BF\u09B6\u09C0\u09B2\u09A8 \u0995\u09B0\u09C7 \u09A8\
  \u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A1\u09BE\u099F\u09BE \u09AC\
  \u09C7\u09B0 \u0995\u09B0\u09BE \u09AC\u09BE \u0993\u09AF\u09BC\u09C7\u09AC-\u09B8\
  \u09AE\u09CD\u09AA\u09B0\u09CD\u0995\u09BF\u09A4 \u0995\u09BE\u099C \u0985\u099F\
  \u09CB\u09AE\u09C7\u09B6\u09A8\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BE \u0995\u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.272251-06:00'
model: gpt-4-0125-preview
summary: "PowerShell-\u098F HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 HTML \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F\
  \u0995\u09C7 \u09AA\u09B0\u09BF\u09B6\u09C0\u09B2\u09A8 \u0995\u09B0\u09C7 \u09A8\
  \u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A1\u09BE\u099F\u09BE \u09AC\
  \u09C7\u09B0 \u0995\u09B0\u09BE \u09AC\u09BE \u0993\u09AF\u09BC\u09C7\u09AC-\u09B8\
  \u09AE\u09CD\u09AA\u09B0\u09CD\u0995\u09BF\u09A4 \u0995\u09BE\u099C \u0985\u099F\
  \u09CB\u09AE\u09C7\u09B6\u09A8\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BE \u0995\u09B0\u09C7\u2026"
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কি ও কেন?
PowerShell-এ HTML পার্স করা মানে HTML কন্টেন্টকে পরিশীলন করে নির্দিষ্ট ডাটা বের করা বা ওয়েব-সম্পর্কিত কাজ অটোমেশনে ব্যবহার করা। প্রোগ্রামাররা এটা করে ওয়েব পেজের সাথে যোগাযোগ করতে, ওয়েব কন্টেন্ট স্ক্র্যাপ করতে, অথবা ফর্ম সাবমিশন ও অন্যান্য ওয়েব ইন্টার‍্যাকশন অটোমেট করতে ওয়েব ব্রাউজারের প্রয়োজন ছাড়াই।

## কিভাবে:

PowerShell স্বাভাবিকভাবে একটি নিবেদিত HTML পার্সার সম্বলিত নয়, তবে আপনি `Invoke-WebRequest` cmdlet ব্যবহার করে HTML কন্টেন্ট প্রবেশ করে এবং পার্স করতে পারেন। জটিল পার্সিং এবং ম্যানিপুলেশনের জন্য, HtmlAgilityPack, একটি জনপ্রিয় .NET লাইব্রেরি, ব্যবহৃত হতে পারে।

### `Invoke-WebRequest` ব্যবহার করে:

```powershell
# ওয়েবপেজ থেকে টাইটেল আনতে সিম্পল উদাহরণ
$response = Invoke-WebRequest -Uri 'http://example.com'
# DOM এলিমেন্টসে প্রবেশ করতে ParsedHtml প্রোপার্টি ব্যবহার করুন
$title = $response.ParsedHtml.title
Write-Output $title
```

নমুনা আউটপুট:

```
Example Domain
```

### HtmlAgilityPack ব্যবহার করে:

প্রথমে, আপনাকে HtmlAgilityPack ইনস্টল করতে হবে। আপনি এটি NuGet প্যাকেজ ম্যানেজার মারফত করতে পারেন:

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

তারপর, আপনি এটি PowerShell-এ HTML পার্স করতে ব্যবহার করতে পারেন:

```powershell
# HtmlAgilityPack অ্যাসেমব্লি লোড করুন
Add-Type -Path "path\to\HtmlAgilityPack.dll"

# একটি HtmlDocument অবজেক্ট তৈরি করুন
$doc = New-Object HtmlAgilityPack.HtmlDocument

# একটি ফাইল অথবা ওয়েব রিকোয়েস্ট থেকে HTML লোড করুন
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# XPath অথবা অন্যান্য কোয়েরি পদ্ধতিগুলি ব্যবহার করে এলিমেন্টস এক্সট্র্যাক্ট করুন
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

নমুনা আউটপুট:

```
Welcome to Example.com!
```

এই উদাহরণগুলিতে, সিম্পল টাস্কগুলির জন্য `Invoke-WebRequest` সর্বোত্তম, অপরদিকে HtmlAgilityPack জটিল HTML পার্সিং ও ম্যানিপুলেশনের জন্য অনেক বৈচিত্র্যময় সেট সুবিধাজনক অফার করে।
