---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:32.925517-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PowerShell \u098F JSON \u09AA\u09A1\
  \u09BC\u09A4\u09C7 \u09AC\u09BE \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09A4\
  \u09C7, \u0986\u09AA\u09A8\u09BF `ConvertFrom-Json` cmdlet \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964\
  \ JSON \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A6\u09C7\u0993\u09AF\u09BC\
  \u09BE \u09B9\u09B2\u09C7, \u098F\u0987 cmdlet \u098F\u099F\u09BF \u098F\u0995\u099F\
  \u09BF PowerShell\u2026"
lastmod: '2024-03-17T18:47:44.300247-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u098F JSON \u09AA\u09A1\u09BC\u09A4\u09C7 \u09AC\u09BE \u09AA\
  \u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\u09BF `ConvertFrom-Json`\
  \ cmdlet \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u09A8\u0964 JSON \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A6\
  \u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u09C7, \u098F\u0987 cmdlet \u098F\u099F\
  \u09BF \u098F\u0995\u099F\u09BF PowerShell \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\
  \u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09C7\u0964\
  ."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কিভাবে:


### JSON পার্সিং
PowerShell এ JSON পড়তে বা পার্স করতে, আপনি `ConvertFrom-Json` cmdlet ব্যবহার করতে পারেন। JSON স্ট্রিং দেওয়া হলে, এই cmdlet এটি একটি PowerShell অবজেক্টে রূপান্তর করে।

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

নমুনা আউটপুট:

```
John Doe
```

এই উদাহরণ দেখায় কিভাবে একটি সাধারণ JSON স্ট্রিং পার্স করে প্রাপ্ত অবজেক্টের প্রপার্টি এ্যাক্সেস করা যায়।

### JSON জেনারেটিং
একটি PowerShell অবজেক্ট থেকে JSON জেনারেট করতে, আপনি `ConvertTo-Json` cmdlet ব্যবহার করতে পারেন। এটি ডাটা ওয়েব সার্ভিসে পাঠানোর বা কনফিগারেশন ফাইলে সংরক্ষণের জন্য সুবিধাজনক।

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Angeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

নমুনা আউটপুট:

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Angeles"
}
```

এই কোড স্নিপেটটি একটি PowerShell অবজেক্ট তৈরি করে এবং তারপর এটিকে JSON স্ট্রিং এ রূপান্তর করে।
