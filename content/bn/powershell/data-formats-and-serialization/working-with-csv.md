---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:30.666605-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: CSV \u09AB\u09BE\u0987\u09B2 \u09A5\
  \u09C7\u0995\u09C7 \u09AA\u09A1\u09BC\u09A4\u09C7, `Import-Csv` cmdlet \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8\u0964 \u098F\u0987 cmdlet\
  \ \u099F\u09BF \u09AB\u09BE\u0987\u09B2\u099F\u09BF \u09AA\u09A1\u09BC\u09C7 \u098F\
  \u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u099F\u09BF \u09B8\u09BE\u09B0\u09BF\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09BE\u09B8\u09CD\u099F\u09AE \u09AA\u09BE\
  \u0993\u09AF\u09BC\u09BE\u09B0\u09B6\u09C7\u09B2 \u0985\u09AC\u099C\u09C7\u0995\u09CD\
  \u099F\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09C7\
  \u0964."
lastmod: '2024-03-17T18:47:44.301441-06:00'
model: gpt-4-0125-preview
summary: "CSV \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09AA\u09A1\u09BC\
  \u09A4\u09C7, `Import-Csv` cmdlet \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C1\u09A8\u0964 \u098F\u0987 cmdlet \u099F\u09BF \u09AB\u09BE\u0987\u09B2\
  \u099F\u09BF \u09AA\u09A1\u09BC\u09C7 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\
  \u09BF\u099F\u09BF \u09B8\u09BE\u09B0\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\
  \u09BE\u09B8\u09CD\u099F\u09AE \u09AA\u09BE\u0993\u09AF\u09BC\u09BE\u09B0\u09B6\u09C7\
  \u09B2 \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u09C7 \u09B0\u09C2\u09AA\u09BE\
  \u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09C7\u0964."
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কিভাবে:


### একটি CSV ফাইল পড়া
CSV ফাইল থেকে পড়তে, `Import-Csv` cmdlet ব্যবহার করুন। এই cmdlet টি ফাইলটি পড়ে এবং প্রতিটি সারির জন্য কাস্টম পাওয়ারশেল অবজেক্টে রূপান্তর করে।

```powershell
# একটি CSV ফাইল ইম্পোর্ট করা
$data = Import-Csv -Path "C:\Data\users.csv"
# কনটেন্ট দেখান
$data
```

**স্যাম্পল আউটপুট:**

```
নাম    বয়স    শহর
----    ---    ----
জন    23     নিউ ইয়র্ক
ডো     29     লস এঞ্জেলেস
```

### একটি CSV ফাইলে লিখন
অন্যদিকে, ডেটা একটি CSV ফাইলে লিখতে, `Export-Csv` cmdlet ব্যবহার করা হয়। এই cmdlet ইনপুট অবজেক্টগুলিকে CSV ফরম্যাটে রূপান্তর করে।

```powershell
# রপ্তানি করার জন্য একটি অবজেক্ট তৈরি করা
$users = @(
    [PSCustomObject]@{Name='জন'; Age='23'; City='নিউ ইয়র্ক'},
    [PSCustomObject]@{Name='ডো'; Age='29'; City='লস এঞ্জেলেস'}
)

# একটি CSV ফাইলে রপ্তানি করা
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

এই কাজ সম্পন্ন করার পর, `new_users.csv` নামের একটি ফাইল প্রদত্ত ডেটা সহ তৈরি হয়।

### CSV কনটেন্ট ফিল্টারিং এবং ম্যানিপুলেট করা
CSV ফাইল থেকে ডেটা ফিল্টার বা পরিবর্তন করতে, পাওয়ারশেলের অবজেক্ট ম্যানিপুলেশন ক্ষমতা ব্যবহার করুন। উদাহরণস্বরূপ, একটি নির্দিষ্ট বয়সের উর্ধ্বে এবং একটি নির্দিষ্ট শহর থেকে শুধুমাত্র ব্যবহারকারীদের নির্বাচন করতে:

```powershell
# ডেটা ইম্পোর্ট করা এবং ফিল্টারিং করা
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'লস এঞ্জেলেস'
}

# ফিল্টার করা ডেটা দেখান
$filteredData
```

**স্যাম্পল আউটপুট:**

```
নাম    বয়স    শহর
----    ---    ----
ডো     29     লস এঞ্জেলেস
```

### থার্ড-পার্টি লাইব্রেরিগুলি ব্যবহার করা
যদিও পাওয়ারশেলের নেটিভ cmdlets সাধারণত সাধারণ টাস্কগুলির জন্য যথেষ্ট, আরও জটিল অপারেশনগুলোর জন্য তৃতীয় পক্ষের লাইব্রেরিগুলি বা টুল সুবিধাজনক হতে পারে। তবে, পড়া, লিখন, ফিল্টারিং বা সর্টিং এর মতো স্ট্যান্ডার্ড CSV ম্যানিপুলেশনের জন্য, `Import-Csv` এবং `Export-Csv` এর মতো পাওয়ারশেলের বিল্ট-ইন cmdlets সাধারণত অতিরিক্ত লাইব্রেরিগুলি ছাড়াই দৃঢ় কার্যকারিতা প্রদান করে।
