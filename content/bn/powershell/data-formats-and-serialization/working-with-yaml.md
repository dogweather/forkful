---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:19.445580-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PowerShell, \u09A1\u09BF\u09AB\
  \u09B2\u09CD\u099F \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7, YAML \u09AA\u09BE\u09B0\
  \u09CD\u09B8\u09BF\u0982 \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09CB\u09A8\
  \ \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4\
  \ cmdlet \u09B8\u09B9 \u0986\u09B8\u09C7 \u09A8\u09BE, \u0995\u09BF\u09A8\u09CD\u09A4\
  \u09C1 `powershell-yaml` \u09AE\u09A1\u09BF\u0989\u09B2 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0985\u09A5\u09AC\u09BE `ConvertFrom-\u2026"
lastmod: '2024-03-17T18:47:44.299240-06:00'
model: gpt-4-0125-preview
summary: "PowerShell, \u09A1\u09BF\u09AB\u09B2\u09CD\u099F \u09B9\u09BF\u09B8\u09C7\
  \u09AC\u09C7, YAML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u098F\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u0995\u09CB\u09A8 \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09A8\
  \u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 cmdlet \u09B8\u09B9 \u0986\u09B8\u09C7 \u09A8\
  \u09BE, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 `powershell-yaml` \u09AE\u09A1\u09BF\
  \u0989\u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0985\
  \u09A5\u09AC\u09BE `ConvertFrom-Json` \u0995\u09C7 `yq` \u098F\u09B0 \u09AE\u09A4\
  \ \u099F\u09C2\u09B2\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0985\u09AD\u09BF\u09AF\
  \u09CB\u099C\u09A8 \u0995\u09B0\u09C7 YAML \u0995\u09C7 PowerShell \u0985\u09AC\u099C\
  \u09C7\u0995\u09CD\u099F\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0\u09BF\
  \u09A4 \u0995\u09B0\u09C7 YAML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B8\u09B9\
  \u099C\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC\u0964\
  \n\n#."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

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
