---
title:                "একটি ওয়েবপেজ ডাউনলোড করা"
date:                  2024-03-17T17:47:30.512376-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?
ওয়েব পৃষ্ঠা ডাউনলোড করা মানে ওয়েব থেকে এর সামগ্রী নেওয়া। প্রোগ্রামাররা ওয়েব স্ক্রেপিং, অফলাইন দেখার উদ্দেশ্যে, অথবা ওয়েবসাইটের সাথে অটোমেশন ইন্টারঅ্যাকশনের জন্য এটা করে থাকে।

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
