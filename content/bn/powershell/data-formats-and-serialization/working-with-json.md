---
title:                "JSON এর সাথে কাজ করা"
date:                  2024-03-17T18:29:32.925517-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
