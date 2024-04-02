---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:19:39.765143-06:00
description: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09AC\u09C7\u09B8\u09BF\u0995 \u09AA\u09CD\u09B0\u09AE\u09BE\u09A3\u09C0\
  \u0995\u09B0\u09A3\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B9\u099A\u09CD\u099B\
  \u09C7 \u09AF\u0996\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE \u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\
  \u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0\u09C7\u09B0 \u0995\u09BE\u099B\
  \u09C7 \"\u09B9\u09CD\u09AF\u09BE\u09B2\u09CB, \u098F\u099F\u09BE \u0986\u09AE\u09BF\
  \" \u09AC\u09B2\u09C7, \u098F\u0995\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0\u0995\u09BE\u09B0\u09C0 \u09A8\u09BE\u09AE \u098F\u09AC\u0982 \u09AA\u09BE\
  \u09B8\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1\u2026"
lastmod: '2024-03-17T18:47:44.274656-06:00'
model: gpt-4-0125-preview
summary: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09AC\u09C7\u09B8\u09BF\u0995 \u09AA\u09CD\u09B0\u09AE\u09BE\u09A3\u09C0\
  \u0995\u09B0\u09A3\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B9\u099A\u09CD\u099B\
  \u09C7 \u09AF\u0996\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE \u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\
  \u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0\u09C7\u09B0 \u0995\u09BE\u099B\
  \u09C7 \"\u09B9\u09CD\u09AF\u09BE\u09B2\u09CB, \u098F\u099F\u09BE \u0986\u09AE\u09BF\
  \" \u09AC\u09B2\u09C7, \u098F\u0995\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0\u0995\u09BE\u09B0\u09C0 \u09A8\u09BE\u09AE \u098F\u09AC\u0982 \u09AA\u09BE\
  \u09B8\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1\u2026"
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
weight: 45
---

## কী এবং কেন?

HTTP অনুরোধ পাঠানো বেসিক প্রমাণীকরণের সাথে হচ্ছে যখন আপনার প্রোগ্রাম একটি ওয়েব সার্ভারের কাছে "হ্যালো, এটা আমি" বলে, একটি ব্যবহারকারী নাম এবং পাসওয়ার্ড ব্যবহার করে। প্রোগ্রামাররা এটি তারা API বা রিসোর্স অ্যাক্সেস করতে ব্যবহার করে যাদের পরিচয়ের প্রমাণ দরকার হয় - এটি একটি গোপন হাত মিলানোর মতো যা আপনাকে ক্লাবে প্রবেশ করতে দেয়।

## কিভাবে:

এখানে কিভাবে আপনি বেসিক প্রমাণীকরণের মাধ্যমে একটি সার্ভারের কাছে ডেটার জন্য 'দয়া করে' বলে জিজ্ঞাসা করবেন:

```PowerShell
# প্রমাণপত্রগুলো প্রস্তুত করা হচ্ছে
$user = 'YourUsername'
$pass = 'YourPassword'
$pair = "$($user):$($pass)"
$encodedCreds = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($pair))

# হেডারগুলো সেট আপ করা হচ্ছে
$headers = @{
    Authorization = "Basic $encodedCreds"
}

# যে URL এ আপনি টোকা দিচ্ছেন
$url = 'https://api.example.com/data'

# এখন, চলুন কল করি
$response = Invoke-RestMethod -Uri $url -Method Get -Headers $headers

# ফলাফল আউটপুট করা হচ্ছে
$response
```

ধরুন ফলাফল যদি JSON ফরম্যাটে থাকে তাহলে নমুনা আউটপুট এরকম দেখাতে পারে:

```json
{
    "name": "John Doe",
    "email": "john@example.com"
}
```

## গভীর অন্তর্দৃষ্টি

বেসিক প্রমাণীকরণ পুরানো স্কুলের, যেটি ইন্টারনেটের প্রাথমিক দিনগুলিতে ফিরে যায় যখন প্রত্যেকে প্রত্যেককে চিনত। এখনও ব্যবহার হয়ে থাকে, তবে এটি নিজে নিজে সুপার সুরক্ষিত নয় - এটি যেন আপনার গোপন ক্লাবের পাসওয়ার্ড একটি পোস্টকার্ডে পাঠানো। আজকাল, আমরা সাধারণত এটি HTTPS এর উপর দিয়ে পাঠাই যাতে এটি এনক্রিপ্ট করা যায়, যেন এটি একটি লক করা বাক্সে পোস্টকার্ড পাঠানোর মতো।

বিকল্প? প্রচুর। আপনার কাছে API কী, OAuth, বেয়ার টোকেন... তালিকাটি চলতে থাকে। প্রতিটির সাথে আসে নিজস্ব হাত মিলানো এবং গোপন শব্দ।

বাস্তবায়নের দিক থেকে, PowerShell এ, আপনি আপনার ব্যবহারকারী নাম এবং পাসওয়ার্ডটি এমন একটি ফরম্যাটে রূপান্তর করছেন যা HTTP প্রোটোকল বুঝতে পারে – base64। কিন্তু মনে রাখবেন, base64 এনক্রিপ্শন নয়; এটি কেবল একটি ছদ্মবেশে খেলা করা টেক্সট। এটি HTTPS এর উপর দিয়ে না পাঠালে যে কেউ এটি প্রকাশ করতে পারে।

## আরও দেখুন

- [Invoke-RestMethod ডকুমেন্টেশন](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [MDN এ HTTP বেসিক অ্যাক্সেস প্রমাণীকরণ](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Base64 এনকোডিং বুঝতে](https://en.wikipedia.org/wiki/Base64)
- [HTTPS এনক্রিপশন সম্পর্কে তথ্য](https://en.wikipedia.org/wiki/HTTPS)
