---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:36.340985-06:00
description: "\u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2\
  \ \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\
  \ \u09B8\u09CD\u09AC\u09B2\u09CD\u09AA\u09AE\u09C7\u09AF\u09BC\u09BE\u09A6\u09C0\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09BE, \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987 \u098F\u0995\u099F\u09BF\
  \ \u09B8\u09C7\u09B6\u09A8\u09C7\u09B0 \u09B8\u09AE\u09AF\u09BC \u09A1\u09C7\u099F\
  \u09BE \u09B8\u099E\u09CD\u099A\u09AF\u09BC \u0995\u09B0\u09A4\u09C7\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\
  \u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7 \u09AF\u09BE\u09A4\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.298228-06:00'
model: gpt-4-0125-preview
summary: "\u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2\
  \ \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\
  \ \u09B8\u09CD\u09AC\u09B2\u09CD\u09AA\u09AE\u09C7\u09AF\u09BC\u09BE\u09A6\u09C0\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09BE, \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987 \u098F\u0995\u099F\u09BF\
  \ \u09B8\u09C7\u09B6\u09A8\u09C7\u09B0 \u09B8\u09AE\u09AF\u09BC \u09A1\u09C7\u099F\
  \u09BE \u09B8\u099E\u09CD\u099A\u09AF\u09BC \u0995\u09B0\u09A4\u09C7\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\
  \u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7 \u09AF\u09BE\u09A4\u09C7\u2026"
title: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
অস্থায়ী ফাইল তৈরি করা মানে হল স্বল্পমেয়াদী ব্যবহারের জন্য একটি ফাইল তৈরি করা, প্রায়ই একটি সেশনের সময় ডেটা সঞ্চয় করতে। প্রোগ্রামাররা এটি করে থাকে যাতে সিস্টেমকে অপরিচ্ছন্ন না করা যায় এবং যে ডেটাগুলি স্থায়ী থাকার প্রয়োজন নেই সেগুলি সামাল দেয়া যায়।

## কিভাবে:
পাওয়ারশেলে একটি অস্থায়ী ফাইল তৈরি করতে, আপনি `New-TemporaryFile` ব্যবহার করবেন। এই cmdlet আপনার টেম্প ফোল্ডারে একটি অস্থায়ী ফাইল তৈরি করে। এখানে জাদুর মন্ত্রটি:

```PowerShell
$tempFile = New-TemporaryFile
```

এই লাইনটি ডিজিটাল এথার থেকে একটি ব্র্যান্ড নিউ অস্থায়ী ফাইলের আহ্বান করে। জানতে চান এটা কোথায় আছে? শুধু টাইপ করুন:

```PowerShell
$tempFile.FullName
```

এবং ব্যাম! এটি আপনাকে ফাইলের পথ বলে দেবে। যখন আপনি শেষ হয়ে গেছেন এবং পরিষ্কার করতে চান, শুধু এটি মুছে ফেলুন:

```PowerShell
Remove-Item $tempFile.FullName
```

ফাইলটি বিলীন হয়ে যায়, কোনো চিহ্ন ছাড়া।

## গভীরে যাওয়া
এখন, আসুন আমরা আরও গভীরে যাই। ঐতিহাসিকভাবে, অস্থায়ী ফাইলগুলি কম্পিউটিং আবির্ভাবের শুরু থেকেই ব্যবহৃত হয়েছে, মূলত কারণ RAM ছিল দুর্লভ এবং ব্যয়বহুল। এই অস্থায়ী ফাইলগুলি ছিল সীমিত মেমোরির জন্য একটি সমাধান।

বিকল্প হিসেবে, কিছু ডেভ নিজেরা `[System.IO.Path]::GetTempFileName()` ব্যবহার করে তাদের অস্থায়ী ফাইলের পথ তৈরি করে থাকে যা বিভিন্ন .NET-সমর্থিত ভাষায় কাজ করে এবং আপনাকে আরও নিয়ন্ত্রণ দেয়।

পাওয়ারশেলে, `New-TemporaryFile` আসলে এই .NET পদ্ধতির আশেপাশে একটি মার্জিত মোড়ক। এটি `C:\Users\YourName\AppData\Local\Temp\tmpXXXX.tmp` এর মতো একটি পথে একটি ফাইল তৈরি করে (`XXXX` একটি যাদুকরী সংখ্যা)। এক্সটেনশন `.tmp` একটি রীতি, যা একটি অস্থায়ী স্বভাব নির্দেশ করে।

মনে রাখবেন, অস্থায়ী ফাইলগুলি যথাযথভাবে বিনষ্ট করা উচিত। আপনি যদি অনেকগুলি তৈরি করেন অথবা সংবেদনশীল ডেটা নিয়ে কাজ করেন, তবে ডেটা লিক প্রতিরোধের জন্য আপনার উচিত তাদের নিরাপদে পরিষ্কার করা।

## আরো দেখুন
- `New-TemporaryFile` সম্পর্কে আরও জানার জন্য [ডক্স](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile) দেখুন।
- `System.IO.Path` ক্লাসের পদ্ধতিগুলো সম্পর্কে [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=net-6.0) এ গভীরে ডুব দিন।
