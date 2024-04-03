---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:30.512376-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u09AA\u09BE\u0993\u09AF\u09BC\u09BE\u09B0\u09B6\u09C7\u09B2 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u0993\u09AF\
  \u09BC\u09C7\u09AC \u09AA\u09C3\u09B7\u09CD\u09A0\u09BE \u09A8\u09C7\u0993\u09AF\
  \u09BC\u09BE\u09B0 \u099C\u09BE\u09A6\u09C1\u09B0 \u09AE\u09A8\u09CD\u09A4\u09CD\
  \u09B0 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0964 \u0986\u09AE\u09B0\
  \u09BE `Invoke-WebRequest` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09AC\u0964."
lastmod: '2024-03-17T18:47:44.273300-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE\u09B0\
  \u09B6\u09C7\u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C3\u09B7\u09CD\
  \u09A0\u09BE \u09A8\u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u099C\u09BE\u09A6\u09C1\
  \u09B0 \u09AE\u09A8\u09CD\u09A4\u09CD\u09B0 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\
  \ \u09B9\u09B2\u0964 \u0986\u09AE\u09B0\u09BE `Invoke-WebRequest` \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09AC\u0964."
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কিভাবে:
এখানে পাওয়ারশেল ব্যবহার করে একটি ওয়েব পৃষ্ঠা নেওয়ার জাদুর মন্ত্র দেওয়া হল। আমরা `Invoke-WebRequest` ব্যবহার করব।

```PowerShell
# example.com এর সামগ্রী ধরুন
$response = Invoke-WebRequest -Uri "http://example.com"

# এইটা আপনি পেলেন
$response.Content
```

নমুনা আউটপুট:

```PowerShell
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
    <!-- এবং তা সম্পর্কে -->
</head>
...
</html>
```

আপনি কিন্তু শুধু টেক্সট চান, HTML ট্যাগ নয়। চলুন সেটা করি:

```PowerShell
# শুধু টেক্সট, অনুগ্রহ করে
$response.ParsedHtml.body.innerText
```

## গভীর ডুব
একসময়, PowerShell এ `Invoke-WebRequest` cmdlet ছিল না। প্রোগ্রামাররা .NET `System.Net.WebClient` ক্লাস বা বাহ্যিক টূলস ব্যবহার করতেন। এখন, সবকিছু অন্তর্ভুক্ত থাকায়, আমাদের জন্য কাজ সহজ হয়ে যায়।

`Invoke-WebRequest` শুধু সামগ্রী নয়, হেডার, স্ট্যাটাস, এবং সেশন তথ্য - সবকিছু থাকে। যদি আপনি API নিয়ে কাজ করছেন, তাহলে `Invoke-RestMethod` অপশনটি আপনি পছন্দ করবেন কারণ এটি সেই জন্য মনোনীত।

আদ্যোপান্তে, এই cmdlets .NET HttpClient ক্লাসের উপর নির্ভর করে, নির্ভরযোগ্যতা এবং বিস্তৃত ফাংশনালিটি প্রদান করে।

এবং, যদি আপনি ওয়েব পৃষ্ঠা ডাউনলোডের জন্য অধীর আগ্রহে অপেক্ষা করছেন, `Invoke-WebRequest` অ্যাসিঙ্ক্রোনাস অপারেশন সাপোর্ট করে। তবে, সেটা অন্য একদিনের বিষয়।

## আরও দেখুন
- [Invoke-WebRequest ডকুমেন্টেশন](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- API ইন্টারঅ্যাকশনের জন্য [Invoke-RestMethod সম্পর্কে আরো](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- কৌতূহলী কোডারদের জন্য একটি [PowerShell GitHub রেপোজিটরি](https://github.com/PowerShell/PowerShell), যারা মনস্থির করে দেখতে চান।
