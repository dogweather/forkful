---
title:                "ইয়ামেল নিয়ে কাজ করা"
date:                  2024-03-17T18:37:19.445580-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
