---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:04:55.760901-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PowerShell \u09B8\u09CD\u09AC\u09BE\
  \u09AD\u09BE\u09AC\u09BF\u0995\u09AD\u09BE\u09AC\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u09A8\u09BF\u09AC\u09C7\u09A6\u09BF\u09A4 HTML \u09AA\u09BE\u09B0\u09CD\u09B8\
  \u09BE\u09B0 \u09B8\u09AE\u09CD\u09AC\u09B2\u09BF\u09A4 \u09A8\u09AF\u09BC, \u09A4\
  \u09AC\u09C7 \u0986\u09AA\u09A8\u09BF `Invoke-WebRequest` cmdlet \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 HTML \u0995\u09A8\u09CD\u099F\u09C7\u09A8\
  \u09CD\u099F \u09AA\u09CD\u09B0\u09AC\u09C7\u09B6 \u0995\u09B0\u09C7 \u098F\u09AC\
  \u0982 \u09AA\u09BE\u09B0\u09CD\u09B8\u2026"
lastmod: '2024-03-17T18:47:44.272251-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u09B8\u09CD\u09AC\u09BE\u09AD\u09BE\u09AC\u09BF\u0995\u09AD\u09BE\
  \u09AC\u09C7 \u098F\u0995\u099F\u09BF \u09A8\u09BF\u09AC\u09C7\u09A6\u09BF\u09A4\
  \ HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BE\u09B0 \u09B8\u09AE\u09CD\u09AC\u09B2\
  \u09BF\u09A4 \u09A8\u09AF\u09BC, \u09A4\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF `Invoke-WebRequest`\
  \ cmdlet \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 HTML \u0995\
  \u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09AA\u09CD\u09B0\u09AC\u09C7\u09B6\
  \ \u0995\u09B0\u09C7 \u098F\u09AC\u0982 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u099C\u099F\u09BF\u09B2 \u09AA\
  \u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u098F\u09AC\u0982 \u09AE\u09CD\u09AF\u09BE\
  \u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  , HtmlAgilityPack, \u098F\u0995\u099F\u09BF \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\
  \u09AF\u09BC .NET \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF, \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\
  \u0964\n\n#."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

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
