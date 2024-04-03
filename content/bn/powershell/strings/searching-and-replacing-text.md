---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:16:40.367086-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PowerShell \u098F \u0996\u09CB\
  \u0981\u099C\u09BE \u098F\u09AC\u0982 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\
  \u09A8 \u09AC\u09C7\u09B6 \u09B8\u09B0\u09B2\u0964 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 `-replace` \u098F\u09AC\u0982 \u09AB\
  \u09BE\u0987\u09B2\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF `Get-Content` \u09B8\u09B9\
  \ `Set-Content` \u09A6\u09C7\u0996\u09C1\u09A8\u0964 #."
lastmod: '2024-03-17T18:47:44.255761-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u098F \u0996\u09CB\u0981\u099C\u09BE \u098F\u09AC\u0982 \u09AA\
  \u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u09AC\u09C7\u09B6 \u09B8\u09B0\u09B2\
  \u0964 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u09A4\u09C7\
  \ `-replace` \u098F\u09AC\u0982 \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF `Get-Content` \u09B8\u09B9 `Set-Content` \u09A6\u09C7\u0996\u09C1\u09A8\
  \u0964\n\n#."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
weight: 10
---

## কিভাবে:
PowerShell এ খোঁজা এবং পরিবর্তন বেশ সরল। স্ট্রিংগুলিতে `-replace` এবং ফাইলের জন্য `Get-Content` সহ `Set-Content` দেখুন।

### স্ট্রিংয়ে লেখা পরিবর্তন:
```PowerShell
$text = "I love PowerShell"
$updatedText = $text -replace "love", "adore"
$updatedText
```
নমুনা আউটপুট:
```
I adore PowerShell
```

### ফাইলে লেখা পরিবর্তন:
```PowerShell
$file = "example.txt"
$content = Get-Content $file
$content | ForEach-Object { $_ -replace "oldWord", "newWord" } | Set-Content $file
```
এখানে আউটপুট নেই, কিন্তু `example.txt` এ এখন প্রতিটি "oldWord" "newWord" দিয়ে পরিবর্তন হয়ে গেছে।

## গভীর ডুব
টেক্সট এডিটিং শুরু হওয়ার সময় থেকে, খোঁজা এবং পরিবর্তন এই ক্ষেত্রের একটি মূল বিষয় হয়ে উঠেছে। এটি চিন্তা করুন যেন ওয়ার্ড প্রসেসরের ফাইন্ড-এন্ড-রিপ্লেস, কিন্তু কোডিং প্রয়োজনের জন্য সুপারচার্জড।

পুরানো দিনে, কমান্ড-লাইন জাদুকররা Unix এ `sed` এর মতো টুলস ব্যবহার করত। PowerShell এই কার্যকারিতা তার স্ক্রিপ্টিং ভাষায় নিয়ে এসেছে। এটি কেন দারুণ? কারণ এটি শুধু টেক্সটের উপরে নয়, বস্তুর উপরেও প্রযোজ্য। এর মানে হল আপনি শুধু কোড এবং টেক্সট ফাইলগুলিই নয়, ডাটা স্ট্রাকচার এবং তার বাইরেও পরিবর্তন করতে পারবেন।

বিকল্প? অবশ্যই। আপনার কাছে নিজের ফাইন্ড-এন্ড-রিপ্লেস সহ টেক্সট এডিটর এবং IDEs, ব্যাচ স্ক্রিপ্টস, বা এমনকি টেক্সট ম্যানিপুলেশনের জন্য ডিজাইন করা প্রোগ্রামিং লাইব্রেরিগুলি আছে।

বাস্তবায়নের বিস্তারিত? PowerShell রেগেক্স করে। এর মানে হল আপনি শুধু নির্ধারিত শব্দের ভিত্তিতে নয়, প্যাটার্ন অনুযায়ী জিনিস পরিবর্তন করতে পারেন। প্লাস, PowerShell স্ক্রিপ্টসের সাথে, আপনি অসংখ্য ফাইলে এই অপারেশনগুলি অটোমেট করতে পারেন, যা আপনাকে একটা বড় সময় সাশ্রয় করতে সাহায্য করে।

## আরো দেখুন
- PowerShell `-replace` অপারেটর ডকুমেন্টেশন: [লিংক](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators)
- `Get-Content` এবং `Set-Content` ব্যবহার: [লিংক](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
