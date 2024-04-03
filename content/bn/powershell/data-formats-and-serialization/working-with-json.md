---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:32.925517-06:00
description: "PowerShell \u098F\u09B0 JSON (JavaScript Object Notation) \u098F\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u098F\u0995\u09C0\u09AD\u09C2\u09A4\u0995\u09B0\u09A3\
  \ \u09B9\u09B2 JSON \u09A1\u09BE\u099F\u09BE \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\
  \u09B0\u09BE (\u09AA\u09A1\u09BC\u09BE) \u098F\u09AC\u0982 \u099C\u09C7\u09A8\u09BE\
  \u09B0\u09C7\u099F \u0995\u09B0\u09BE (\u09B2\u09C7\u0996\u09BE) \u09A8\u09BF\u09AF\
  \u09BC\u09C7, \u09AF\u09BE \u0993\u09AF\u09BC\u09C7\u09AC\u09C7 \u09A1\u09BE\u099F\
  \u09BE \u09AC\u09BF\u09A8\u09BF\u09AE\u09AF\u09BC\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.300247-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u098F\u09B0 JSON (JavaScript Object Notation) \u098F\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u098F\u0995\u09C0\u09AD\u09C2\u09A4\u0995\u09B0\u09A3 \u09B9\
  \u09B2 JSON \u09A1\u09BE\u099F\u09BE \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\
  \u09BE (\u09AA\u09A1\u09BC\u09BE) \u098F\u09AC\u0982 \u099C\u09C7\u09A8\u09BE\u09B0\
  \u09C7\u099F \u0995\u09B0\u09BE (\u09B2\u09C7\u0996\u09BE) \u09A8\u09BF\u09AF\u09BC\
  \u09C7, \u09AF\u09BE \u0993\u09AF\u09BC\u09C7\u09AC\u09C7 \u09A1\u09BE\u099F\u09BE\
  \ \u09AC\u09BF\u09A8\u09BF\u09AE\u09AF\u09BC\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u09AB\u09B0\u09AE\
  \u09CD\u09AF\u09BE\u099F\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u0993\u09AF\u09BC\u09C7\u09AC APIs \u098F\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200D\u09CD\u09AF\u09BE\u0995\
  \u09CD\u099F \u0995\u09B0\u09A4\u09C7, \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\
  \u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09A4\u09C7, \u0985\u09A5\u09AC\u09BE \u09AD\u09BF\
  \u09A8\u09CD\u09A8 \u09AD\u09BE\u09B7\u09BE \u098F\u09AC\u0982 \u09AA\u09CD\u09B2\
  \u09CD\u09AF\u09BE\u099F\u09AB\u09B0\u09CD\u09AE\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\
  \u09C7 \u09A1\u09BE\u099F\u09BE \u09AC\u09BF\u09A8\u09BF\u09AE\u09AF\u09BC \u09B8\
  \u09B9\u099C \u0995\u09B0\u09A4\u09C7 JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u0995\u09BE\
  \u09B0\u09A3 \u098F\u099F\u09BF \u09B9\u09BE\u09B2\u0995\u09BE \u098F\u09AC\u0982\
  \ \u09AD\u09BE\u09B7\u09BE-\u09B8\u09CD\u09AC\u09BE\u09A7\u09C0\u09A8 \u09AA\u09CD\
  \u09B0\u0995\u09C3\u09A4\u09BF\u09B0\u0964."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কি এবং কেন?

PowerShell এর JSON (JavaScript Object Notation) এর সাথে একীভূতকরণ হল JSON ডাটা পার্স করা (পড়া) এবং জেনারেট করা (লেখা) নিয়ে, যা ওয়েবে ডাটা বিনিময়ের জন্য একটি সাধারণ ফরম্যাট। প্রোগ্রামাররা ওয়েব APIs এর সাথে ইন্টার‍্যাক্ট করতে, কনফিগারেশন ফাইলের সাথে কাজ করতে, অথবা ভিন্ন ভাষা এবং প্ল্যাটফর্মের মধ্যে ডাটা বিনিময় সহজ করতে JSON এর সাথে কাজ করে থাকেন কারণ এটি হালকা এবং ভাষা-স্বাধীন প্রকৃতির।

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
