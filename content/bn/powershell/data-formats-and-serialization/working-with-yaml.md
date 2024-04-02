---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:19.445580-06:00
description: "YAML, \u0985\u09A5\u09AC\u09BE YAML Ain't Markup Language, \u098F\u0995\
  \u099F\u09BF \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\
  \u09AF \u09A1\u09C7\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\
  \u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AD\u09BE\u09B7\u09BE\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AA\u09CD\u09B0\
  \u09BE\u09AF\u09BC\u0987 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8\
  \ \u09AB\u09BE\u0987\u09B2 \u098F\u09AC\u0982 \u09AD\u09BE\u09B7\u09BE\u09B0 \u09AE\
  \u09A7\u09CD\u09AF\u09C7 \u09A1\u09C7\u099F\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.299240-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u0985\u09A5\u09AC\u09BE YAML Ain't Markup Language, \u098F\u0995\u099F\
  \u09BF \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF\
  \ \u09A1\u09C7\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\
  \u0987\u099C\u09C7\u09B6\u09A8 \u09AD\u09BE\u09B7\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AA\u09CD\u09B0\u09BE\
  \u09AF\u09BC\u0987 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8\
  \ \u09AB\u09BE\u0987\u09B2 \u098F\u09AC\u0982 \u09AD\u09BE\u09B7\u09BE\u09B0 \u09AE\
  \u09A7\u09CD\u09AF\u09C7 \u09A1\u09C7\u099F\u09BE\u2026"
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

## কি এবং কেন?
YAML, অথবা YAML Ain't Markup Language, একটি মানব-পাঠযোগ্য ডেটা সিরিয়ালাইজেশন ভাষা। প্রোগ্রামাররা প্রায়ই কনফিগারেশন ফাইল এবং ভাষার মধ্যে ডেটা ট্রান্সমিশনের জন্য এটি ব্যবহার করে থাকেন। এর সাধারণতা এবং পাঠযোগ্যতা এটিকে পরিবেশ, অ্যাপ্লিকেশন, অথবা সেবাগুলি সেটআপের কাজে বিশেষভাবে জনপ্রিয় করেছে, যেখানে কনফিগারেশনগুলি জরুরী এবং সহজে বোঝা এবং সম্পাদনা করা উচিত।

## কিভাবে:
PowerShell, ডিফল্ট হিসেবে, YAML পার্সিং এর জন্য কোন অন্তর্নির্মিত cmdlet সহ আসে না, কিন্তু `powershell-yaml` মডিউল ব্যবহার করে অথবা `ConvertFrom-Json` কে `yq` এর মত টূলের সাথে অভিযোজন করে YAML কে PowerShell অবজেক্টে রূপান্তরিত করে YAML এর সাথে সহজে কাজ করা যায়।

### `powershell-yaml` মডিউল ব্যবহার করে:
প্রথমে, মডিউল ইনস্টল করুন:
```PowerShell
Install-Module -Name powershell-yaml
```

YAML ফাইল পড়ার জন্য:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

PowerShell অবজেক্টটি YAML ফাইলে লেখার জন্য:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

স্যাম্পল `output.yml`:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### `yq` এবং `ConvertFrom-Json` ব্যবহার করে YAML পার্সিং:
অন্য একটি পদ্ধতি হল `yq` ব্যবহার করা, যা একটি হালকা এবং পোর্টেবল কমান্ড লাইন YAML প্রসেসর। `yq` YAML কে JSON এ রূপান্তরিত করতে পারে, যা PowerShell স্বাভাবিকভাবে পার্স করতে পারে।

প্রথমে, নিশ্চিত করুন `yq` আপনার সিস্টেমে ইনস্টল করা আছে।
তারপর রান করুন:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

এই পদ্ধতি বিশেষত PowerShell এর মধ্যে JSON ব্যবহারের জন্য পছন্দ করেন এমন ব্যবহারকারীদের জন্য উপকারী, যারা ক্রস-প্ল্যাটফর্ম পরিবেশে কাজ করে থাকেন।
