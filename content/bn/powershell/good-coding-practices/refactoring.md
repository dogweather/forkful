---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:12:08.897539-06:00
description: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982\
  \ \u09B9\u09B2 \u0986\u0997\u09C7 \u09A5\u09C7\u0995\u09C7 \u09B2\u09C7\u0996\u09BE\
  \ \u0995\u09AE\u09CD\u09AA\u09BF\u0989\u099F\u09BE\u09B0 \u0995\u09CB\u09A1 \u09AA\
  \u09C1\u09A8\u09B0\u09CD\u0997\u09A0\u09A8\u09C7\u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\
  \u09B0\u09BF\u09AF\u09BC\u09BE, \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u098F\u09B0\
  \ \u09AC\u09BE\u0987\u09B0\u09C7\u09B0 \u0986\u099A\u09B0\u09A3 \u09AA\u09B0\u09BF\
  \u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC \u09A8\u09BE\
  , \u0989\u09A6\u09CD\u09A6\u09C7\u09B6\u09CD\u09AF \u09B8\u09AB\u09CD\u099F\u0993\
  \u09AF\u09BC\u09CD\u09AF\u09BE\u09B0\u09C7\u09B0 \u0985-\u0995\u09BE\u09B0\u09CD\
  \u09AF\u0995\u09B0\u09C0 \u09AC\u09C8\u09B6\u09BF\u09B7\u09CD\u099F\u09CD\u09AF\u2026"
lastmod: '2024-03-17T18:47:44.285793-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982\
  \ \u09B9\u09B2 \u0986\u0997\u09C7 \u09A5\u09C7\u0995\u09C7 \u09B2\u09C7\u0996\u09BE\
  \ \u0995\u09AE\u09CD\u09AA\u09BF\u0989\u099F\u09BE\u09B0 \u0995\u09CB\u09A1 \u09AA\
  \u09C1\u09A8\u09B0\u09CD\u0997\u09A0\u09A8\u09C7\u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\
  \u09B0\u09BF\u09AF\u09BC\u09BE, \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u098F\u09B0\
  \ \u09AC\u09BE\u0987\u09B0\u09C7\u09B0 \u0986\u099A\u09B0\u09A3 \u09AA\u09B0\u09BF\
  \u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC \u09A8\u09BE\
  , \u0989\u09A6\u09CD\u09A6\u09C7\u09B6\u09CD\u09AF \u09B8\u09AB\u09CD\u099F\u0993\
  \u09AF\u09BC\u09CD\u09AF\u09BE\u09B0\u09C7\u09B0 \u0985-\u0995\u09BE\u09B0\u09CD\
  \u09AF\u0995\u09B0\u09C0 \u09AC\u09C8\u09B6\u09BF\u09B7\u09CD\u099F\u09CD\u09AF\u2026"
title: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982"
weight: 19
---

## কি এবং কেন?
রিফ্যাক্টরিং হল আগে থেকে লেখা কম্পিউটার কোড পুনর্গঠনের প্রক্রিয়া, যেখানে এর বাইরের আচরণ পরিবর্তন করা হয় না, উদ্দেশ্য সফ্টওয়্যারের অ-কার্যকরী বৈশিষ্ট্য উন্নত করা। প্রোগ্রামাররা কোড রিফ্যাক্টর করে তা আরও পরিষ্কার, দক্ষ এবং বোঝার সহজ করে তোলে, যা পরবর্তী রক্ষণাবেক্ষণ ও সম্ভাব্য উন্নতিতে সহায়ক হয়।

## কিভাবে:
PowerShell-এ কোন নির্দিষ্ট রিফ্যাক্টরিং টুল বিল্ট-ইন নেই, তবে পাঠযোগ্যতা ও দক্ষতা বৃদ্ধির জন্য এখনও আপনি আপনার কোড পরিষ্কার করতে পারেন। যদি এমন একটি ফাংশন থাকে যা অনেক কিছু করে, তবে কিভাবে আমরা তা স্পষ্টতার জন্য রিফ্যাক্টর করতে পারি তা বিবেচনা করা:

```PowerShell
function Get-InventoryData {
    # মৌলিক ফাংশন ডেটা পুনরুদ্ধার এবং ফর্ম্যাটিং সংযোজন
    $data = Get-Content -Path 'C:\inventory-list.txt'
    $inventoryData = $data | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
    $inventoryData | Format-Table -AutoSize
}

# পৃথক ফাংশনে রিফ্যাক্টর করা হয়েছে
function Import-InventoryData {
    param($Path)
    Get-Content -Path $Path | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
}

function Format-InventoryData {
    param($Data)
    $Data | Format-Table -AutoSize
}

# ব্যবহার
$inventory = Import-InventoryData -Path 'C:\inventory-list.txt'
Format-InventoryData -Data $inventory
```

নমুনা আউটপুট:

```
ItemID Name            Count Price
------ ----            ----- -----
1001   Widget Type A   50    9.99
1002   Gadget Type B   20    14.99
```

## গভীর ডাইভ
প্রোগ্রামিং এ রিফ্যাক্টরিং এর মূল সফ্টওয়ার ডেভেলপমেন্টের প্রাথমিক দিনগুলি পর্যন্ত গড়ায়, যদিও ১৯৯০ এর দশকে একটি অনুশীলন হিসেবে এটি সংগঠিত হয়েছিল। মার্টিন ফাউলারের বই "Refactoring: Improving the Design of Existing Code" বিষয়ে একটি প্রধান কাজ, যা পরিষ্কার কোড অর্জনে রিফ্যাক্টরিং এর গুরুত্ব জোর দেয়।

যদিও PowerShell অন্যান্য ভাষার জন্যে কিছু ইন্টিগ্রেটেড ডেভেলপমেন্ট এনভায়রনমেন্টস (IDEs) যেমন Eclipse বা Visual Studio এর মতো নির্দিষ্ট রিফ্যাক্টরিং টুলস সহ আসে না, আপনি ম্যানুয়ালি ভালো রিফ্যাক্টরিং নীতিগুলি অনুশীলন করতে পারেন। মুখ্য ব্যাপার হল রিফ্‌যাক্টরিং শুধুমাত্র কোড পরিবর্তনের জন্য নয়, বরং এমন আচরণ-সংরক্ষণকারী পরিবর্তন করা যা কোডের গঠন এবং নকশাকে উন্নত করে।

PowerShell-এ ম্যানুয়াল রিফ্যাক্টরিং বিকল্পগুলিতে Visual Studio Code সহ ভাষাটি সমর্থন করে এমন IDEs ব্যবহার করা অন্তর্ভুক্ত, যা কোড ফর্ম্যাটিং ও বেসিক রিফ্যাক্টরিং ক্ষমতার মতো বৈশিষ্ট্য অফার করে। আরও গুরুত্বপূর্ণ রিফ্যাক্টরিংয়ের জন্য, আপনি Pester টেস্টগুলি ব্যবহার করে পরিবর্তনগুলি ফাংশনালিটি পরিবর্তন করে না তা নিশ্চিত করতে বিবেচনা করতে পারেন।

উপরন্তু, রিফ্যাক্টরিং বাস্তবায়ন আরও ব্যবস্থাগত পরিবর্তনগুলি যেমন মডিউলারাইজেশন জড়িত হতে পারে, যেখানে কোডকে পুনরায়-ব্যবহৃত মডিউল বা ফাংশনে বিভক্ত করা হয়, DRY (Don't Repeat Yourself) নীতিতে আরও ভালোভাবে মেনে চলার উন্নতি ঘটে। অন্যান্য প্রচলিত রিফ্যাক্টরিং কৌশলগুলির মধ্যে রয়েছে পরিষ্কারতার জন্য পুনঃনামকরণ, ডুপ্লিকেট কোড অপসারণ করা, এবং শর্তাধীন লজিকের জটিলতা কমানো।

## আরও দেখুন
আরও গভীরে যেতে, এখানে কিছু সম্পদ:

- মার্টিন ফাউলারের রিফ্যাক্টরিং বই: [_Refactoring: Improving the Design of Existing Code_](https://martinfowler.com/books/refactoring.html)
- Pester দিয়ে রিফ্যাক্টর কোড পরীক্ষা: [Pester Testing Framework](https://pester.dev/)
- PowerShell সেরা অনুশীলন: [The PowerShell Best Practices and Style Guide](https://poshcode.gitbooks.io/powershell-practice-and-style/)
