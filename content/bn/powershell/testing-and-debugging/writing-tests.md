---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:00.404297-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PowerShell-\u098F \u09AC\u09BF\
  \u09B2\u09CD\u099F-\u0987\u09A8 \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982 \u09AB\
  \u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995 \u09A8\u09C7\u0987\
  , \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 Pester, \u098F\u0995\u099F\u09BF \u099C\u09A8\
  \u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\
  \u0995\u09CD\u09B7\u09C7\u09B0 \u09AE\u09A1\u09BF\u0989\u09B2, \u09B2\u09C7\u0996\
  \u09BE \u0993 \u09B0\u09BE\u09A8 \u0995\u09B0\u09BE \u099F\u09C7\u09B8\u09CD\u099F\
  \u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09BE\u09AA\u0995\u09AD\
  \u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4\u2026"
lastmod: '2024-03-17T18:47:44.280239-06:00'
model: gpt-4-0125-preview
summary: "PowerShell-\u098F \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u099F\u09C7\
  \u09B8\u09CD\u099F\u09BF\u0982 \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\
  \u09B0\u09CD\u0995 \u09A8\u09C7\u0987, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 Pester,\
  \ \u098F\u0995\u099F\u09BF \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09A4\
  \u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09AE\u09A1\
  \u09BF\u0989\u09B2, \u09B2\u09C7\u0996\u09BE \u0993 \u09B0\u09BE\u09A8 \u0995\u09B0\
  \u09BE \u099F\u09C7\u09B8\u09CD\u099F\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\
  \u09CD\u09AF\u09BE\u09AA\u0995\u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09C3\u09A4 \u09B9\u09AF\u09BC\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0986\u09AA\
  \u09A8\u09BE\u09B0 PowerShell \u09AB\u09BE\u0982\u09B6\u09A8 \u099F\u09C7\u09B8\u09CD\
  \u099F \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF Pester \u09A6\u09BF\u09AF\
  \u09BC\u09C7 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE\u09B0 \u0989\u09AA\u09BE\
  \u09AF\u09BC \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কিভাবে:
PowerShell-এ বিল্ট-ইন টেস্টিং ফ্রেমওয়ার্ক নেই, কিন্তু Pester, একটি জনপ্রিয় তৃতীয়-পক্ষের মডিউল, লেখা ও রান করা টেস্টের জন্য ব্যাপকভাবে ব্যবহৃত হয়। এখানে আপনার PowerShell ফাংশন টেস্ট করার জন্য Pester দিয়ে শুরু করার উপায় দেওয়া হল:

প্রথমে, আপনি যদি এখনও Pester ইনস্টল করেননি তবে ইনস্টল করুন:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

এরপর, ধরুন আপনি একটি সাধারণ PowerShell ফাংশন টেস্ট করতে চান, যা `MyFunction.ps1` হিসাবে সেভ করা আছে:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

এই ফাংশনটি Pester দিয়ে টেস্ট করার জন্য, `MyFunction.Tests.ps1` নামে একটি টেস্ট স্ক্রিপ্ট তৈরি করুন। এই স্ক্রিপ্টে, Pester-এর `Describe` এবং `It` ব্লকগুলি ব্যবহার করে টেস্ট কেসগুলি ডিফাইন করুন:

```powershell
# টেস্ট করার জন্য ফাংশন ইম্পোর্ট করুন
. .\MyFunction.ps1

Describe "Get-MultipliedNumber tests" {
    It "কোন গুণকক প্রদান না করা হলে ২ দ্বারা সংখ্যা গুণ করে" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "প্রদানকৃত গুণক দ্বারা সঠিকভাবে সংখ্যা গুণ করে" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

টেস্টগুলো চালাতে, PowerShell খুলুন, আপনার টেস্ট স্ক্রিপ্ট যে ডিরেক্টরিতে আছে সেখানে নেভিগেট করুন, এবং `Invoke-Pester` কমান্ড ব্যবহার করুন:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

নমুনা আউটপুট এই রকম দেখাবে, যা নির্দেশ করবে আপনার টেস্টগুলো পাস হয়েছে কিনা বা ব্যর্থ হয়েছে:

```
Starting discovery in 1 files.
Discovery finished in 152ms.
[+] C:\path\to\MyFunction.Tests.ps1 204ms (182ms|16ms)
Tests completed in 204ms
Tests Passed: 2, Failed: 0, Skipped: 0 NotRun: 0
```

এই আউটপুট দেখিয়ে দেয় যে উভয় টেস্টই পাস হয়েছে, যা আপনাকে আস্থা দেয় যে আপনার `Get-MultipliedNumber` ফাংশন আপনি যে পরিস্থিতিগুলি পরীক্ষা করেছেন তাতে প্রেক্ষাপট অনুযায়ী আচরণ করছে।
