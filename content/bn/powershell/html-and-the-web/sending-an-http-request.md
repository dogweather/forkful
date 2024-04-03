---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:18:03.013888-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0995\u099F\u09BF \u09B8\
  \u09B0\u09B2 GET \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\
  \u09A3\u09C7\u09B0 \u09B8\u09CB\u099C\u09BE \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\
  \u099F\u09BF \u09B9\u09B2."
lastmod: '2024-03-17T18:47:44.270897-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09B0\u09B2 GET \u0985\u09A8\u09C1\u09B0\
  \u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\u09C7\u09B0 \u09B8\u09CB\u099C\
  \u09BE \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u099F\u09BF \u09B9\u09B2."
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

## কিভাবে:
একটি সরল GET অনুরোধ প্রেরণের সোজা পদ্ধতিটি হল:

```PowerShell
$response = Invoke-RestMethod -Uri 'https://api.example.com/data' -Method Get
Write-Output $response
```

এবং, যদি আপনি কিছু তথ্য POST করতে চান:

```PowerShell
$body = @{
    'name' = 'Jane Doe'
    'occupation' = 'Space Ranger'
}

$response = Invoke-RestMethod -Uri 'https://api.example.com/users' -Method Post -Body ($body | ConvertTo-Json)
Write-Output $response
```

নমুনা আউটপুট:

```
name         occupation
----         ----------
Jane Doe     Space Ranger
```

## গভীরে যাও:
HTTP অনুরোধ প্রেরণ ওয়েব উন্নয়নের প্রথম যুগ থেকে চলে আসছে। আপনি HTTP, ওয়েবের নিজস্ব ভাষায় ওয়েবের সাথে একটি সংলাপে জড়িত হচ্ছেন। PowerShell-এর `Invoke-RestMethod` cmdlet এখানে পছন্দের টুল। `Invoke-RestMethod`-এর আগে, `Invoke-WebRequest` ছিল যাবতীয় প্রতিক্রিয়ার জন্য প্রাথমিক পছন্দ এবং এটি এখনো বেশি বিস্তারিত প্রতিক্রিয়ার জন্য ব্যবহার করা হয়।

আপনি যদি সাহসী মনে হোন, তবে `curl` অথবা .NET-এর `HttpClient` ক্লাসের মতো বিকল্পগুলি রয়েছে। `Invoke-RestMethod` ব্যবহার করার সময় মনে রাখবেন এটি .NET-এর `HttpClient` ক্লাস এবং পদ্ধতিগুলির চারপাশে একটি আবরণ, যা সরলতা সরবরাহ করে কিন্তু কিছু নিম্ন-স্তরের নিয়ন্ত্রণ ত্যাগ করে।

বাস্তবায়নের দিক থেকে, মনে রাখবেন HTTP অনুরোধগুলি `GET`, `POST`, `PUT`, ইত্যাদি বিভিন্ন পদ্ধতিতে আসে। `-Headers` দিয়ে হেডার কাস্টমাইজ করুন, এবং অতিরিক্ত প্যারামিটার হিসেবে সময়-আউট এবং প্রমাণীকরণের সাথে পরিচালনা করুন। অনুপ্রবেশ আক্রমণ এড়াতে ব্যবহারকারী-প্রণীত কনটেন্ট ব্যবহার করার সময় সবসময় ইনপুটগুলি স্যানিটাইজ করুন।

## আরও দেখুন:
- [PowerShell-এর Invoke-RestMethod সম্পর্কে](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [`Invoke-WebRequest` বিস্তারিত](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [REST APIs বুঝতে](https://www.redhat.com/en/topics/api/what-is-a-rest-api)
- [`.NET HttpClient` ক্লাস](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
