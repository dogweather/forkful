---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:29.256755-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PowerShell \u098F \u0995\u09CB\
  \u09A8\u09CB \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u0989\
  \u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u09AF\u09BE\u099A\u09BE\u0987 \u0995\
  \u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B8\u09CB\
  \u099C\u09BE\u09B8\u09BE\u09AA\u09CD\u099F\u09BE \u0989\u09AA\u09BE\u09AF\u09BC\
  \ \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7 \u09A4\u09BE \u09B9\u09B2\
  \ `Test-Path` cmdlet \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\
  \u0964 \u098F\u0987 cmdlet \u098F\u0995\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.292270-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u098F \u0995\u09CB\u09A8\u09CB \u09A1\u09BF\u09B0\u09C7\u0995\
  \u09CD\u099F\u09B0\u09BF\u09B0 \u0989\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF\
  \ \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09CB\u099C\u09BE\u09B8\u09BE\u09AA\u09CD\u099F\
  \u09BE \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\
  \u09B0\u09C7 \u09A4\u09BE \u09B9\u09B2 `Test-Path` cmdlet \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u0964 \u098F\u0987 cmdlet \u098F\u0995\u099F\
  \u09BF \u09AC\u09C1\u09B2\u09BF\u09AF\u09BC\u09BE\u09A8 \u09AE\u09BE\u09A8 \u09AB\
  \u09BF\u09B0\u09BF\u09AF\u09BC\u09C7 \u09A6\u09C7\u09AF\u09BC \u09AF\u09BE \u09A8\
  \u09BF\u09B0\u09CD\u09A6\u09C7\u09B6 \u0995\u09B0\u09C7 \u09A8\u09BF\u09B0\u09CD\
  \u09A6\u09BF\u09B7\u09CD\u099F \u09AA\u09A5\u099F\u09BF \u09AC\u09BF\u09A6\u09CD\
  \u09AF\u09AE\u09BE\u09A8 \u0995\u09BF\u09A8\u09BE\u0964 \u098F\u0987\u09AD\u09BE\
  \u09AC\u09C7 \u0986\u09AA\u09A8\u09BF \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কিভাবে:
PowerShell এ কোনো ডিরেক্টরির উপস্থিতি যাচাই করার জন্য একটি সোজাসাপ্টা উপায় প্রদান করে তা হল `Test-Path` cmdlet ব্যবহার করা। এই cmdlet একটি বুলিয়ান মান ফিরিয়ে দেয় যা নির্দেশ করে নির্দিষ্ট পথটি বিদ্যমান কিনা। এইভাবে আপনি এটি ব্যবহার করতে পারেন:

```powershell
# যাচাই করুন কোনো ডিরেক্টরি বিদ্যমান কিনা
$directoryPath = "C:\ExamplePath"
$directoryExists = Test-Path -Path $directoryPath
Write-Output "ডিরেক্টরি বিদ্যমান কিনা? $directoryExists"
```

একটি ডিরেক্টরি যা বিদ্যমান আছে তার জন্য নমুনা আউটপুট:

```
ডিরেক্টরি বিদ্যমান কিনা? True
```

এবং একটি ডিরেক্টরি যা বিদ্যমান নেই:

```
ডিরেক্টরি বিদ্যমান কিনা? False
```

নেটওয়ার্ক শেয়ার বা ক্লাউড স্টোরেজের সাথে যোগাযোগের মতো আরও জটিল স্ক্রিপ্টগুলির ক্ষেত্রে, আপনার অতিরিক্ত যাচাই বা কার্যকারিতা প্রয়োজন হতে পারে যা `Test-Path` এর মাধ্যমে সরাসরি উপলব্ধ নয়। এমন ক্ষেত্রে, তৃতীয় পক্ষের PowerShell মডিউল বা লাইব্রেরীগুলি ব্যবহার করা সুবিধাজনক হতে পারে, যদিও বেশিরভাগ রুটিন কার্যক্রম PowerShell এর অন্তর্ভুক্ত cmdlets দ্বারা সম্পন্ন করা যায়। আমার শেষ জ্ঞান আপডেট পর্যন্ত, `Test-Path` এর বাইরে নির্দিষ্টভাবে ডিরেক্টরি বিদ্যমানতা যাচাই করার জন্য প্রশস্তভাবে গ্রহণযোগ্য কোনো তৃতীয় পক্ষের লাইব্রেরী ছিল না, প্রধানত এর কারণ হল `Test-Path` নিজেই এই উদ্দেশ্যে দৃঢ় এবং দক্ষ।
