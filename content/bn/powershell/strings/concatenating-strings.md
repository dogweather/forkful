---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:40.272447-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF \u098F\
  \u0995\u09A4\u09CD\u09B0\u09BF\u09A4 \u0995\u09B0\u09BE \u09AC\u09BE \u09B8\u0982\
  \u09AF\u09CB\u099C\u09A8, \u09AE\u09C2\u09B2\u09A4 \u09B6\u09AC\u09CD\u09A6\u09C7\
  \u09B0 \u098F\u0995\u099F\u09BF \u099F\u09CD\u09B0\u09C7\u09A8 \u09A4\u09C8\u09B0\
  \u09BF\u09B0 \u09AE\u09A4\u09CB\u0964 \u0986\u09AE\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AE\u09BE\u09A8\u0997\
  \u09C1\u09B2\u09BF \u098F\u0995\u09A4\u09CD\u09B0\u09BF\u09A4 \u0995\u09B0\u09C7\
  , \u09AC\u09BE\u0995\u09CD\u09AF\u09BE\u0982\u09B6, \u09AC\u09BE\u0995\u09CD\u09AF\
  , \u09AC\u09BE \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u09AA\u09C3\u09A5\u0995 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.262970-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF \u098F\
  \u0995\u09A4\u09CD\u09B0\u09BF\u09A4 \u0995\u09B0\u09BE \u09AC\u09BE \u09B8\u0982\
  \u09AF\u09CB\u099C\u09A8, \u09AE\u09C2\u09B2\u09A4 \u09B6\u09AC\u09CD\u09A6\u09C7\
  \u09B0 \u098F\u0995\u099F\u09BF \u099F\u09CD\u09B0\u09C7\u09A8 \u09A4\u09C8\u09B0\
  \u09BF\u09B0 \u09AE\u09A4\u09CB\u0964 \u0986\u09AE\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AE\u09BE\u09A8\u0997\
  \u09C1\u09B2\u09BF \u098F\u0995\u09A4\u09CD\u09B0\u09BF\u09A4 \u0995\u09B0\u09C7\
  , \u09AC\u09BE\u0995\u09CD\u09AF\u09BE\u0982\u09B6, \u09AC\u09BE\u0995\u09CD\u09AF\
  , \u09AC\u09BE \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u09AA\u09C3\u09A5\u0995 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u09B0 \u09B8\u0999\u09CD\
  \u0997\u09C0 \u09B9\u09AF\u09BC\u09C7 \u098F\u0995 \u09B9\u0993\u09AF\u09BC\u09BE\
  \ \u09A6\u09B0\u0995\u09BE\u09B0, \u09B8\u09C7\u09B8\u09AC \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09A4\u09C7\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
weight: 3
---

## কিভাবে:
চলুন সরাসরি তাতে চলে যাই:

```PowerShell
# '+' অপারেটরের মাধ্যমে
$greeting = 'হ্যালো, ' + 'ওয়ার্ল্ড!'
$greeting # আউটপুট: হ্যালো, ওয়ার্ল্ড!

# স্ট্রিং ইন্টারপোলেশনের মাধ্যমে
$name = 'জেন'
$welcomeMessage = "হাই, $name, আপনার সাথে পরিচিত হতে খুব ভালো লাগছে!"
$welcomeMessage # আউটপুট: হাই, জেন, আপনার সাথে পরিচিত হতে খুব ভালো লাগছে!

# '-f' অপারেটরের সাথে (ফরম্যাট অপারেটর)
$city = 'নিউ ইয়র্ক'
$visitMessage = 'স্বাগতম {0}!' -f $city
$visitMessage # আউটপুট: স্বাগতম নিউ ইয়র্ক!

# জটিল পরিস্থিতিগুলির জন্য StringBuilder (সাধারণ জিনিসগুলির জন্য একটু অতিরিক্ত)
$textBuilder = New-Object System.Text.StringBuilder
[void]$textBuilder.Append('পাওয়ারশেল ')
[void]$textBuilder.Append('হল ')
[void]$textBuilder.Append('অসাধারণ.')
$textBuilder.ToString() # আউটপুট: পাওয়ারশেল হল অসাধারণ.
```

## গভীর ডুব
ঐতিহাসিকভাবে, আগের প্রোগ্রামিং ভাষাগুলিতে স্ট্রিংগুলি সংযোজন করা একটু কঠিন ছিল - এটা মনে করুন বাক্যগুলিকে টেপ দিয়ে আটকে রাখার মতো। পাওয়ারশেলে, এটি খুবই সহজ।

কাজটি সম্পন্ন করার বিভিন্ন উপায় আছে। '+' অপারেটরটি সরল কিন্তু অনেকগুলি স্ট্রিংগুলির সাথে এটি ধীর হতে পারে। "$variable" দিয়ে স্ট্রিং ইন্টারপোলেশন পরিষ্কার এবং স্ট্রিংগুলিতে ভ্যারিয়েবল ঢোকানোর জন্য দারুন। টেম্পলেটিং পরিস্থিতিগুলিতে '-f' অপারেটরটি উজ্জ্বল।

পারফরম্যান্স সম্পর্কে - যদি আপনি একটি রচনার সমান স্ট্রিং একত্রিত করতে চান, আপনার কিছু আরও শক্তিশালীর প্রয়োজন হবে। সেই ক্ষেত্রে `StringBuilder` প্রবেশ করে। এটি সাথে সাথে সংযোজন করে না; বরং, এটি আপনার স্ট্রিংগুলিকে ডাকা হলে, সময় ও মেমোরি সাশ্রয় করে একত্রিত করে।

## আরও দেখুন
- [About Join](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_join?view=powershell-7.3)
- [About Automatic Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.3) (দেখুন `$OFS`)
- স্ট্রিং ফরম্যাটিং সম্পর্কে আরও জানতে, দেখুন [Composite Formatting](https://docs.microsoft.com/en-us/dotnet/standard/base-types/composite-formatting).
- এবং, যদি আপনি এটির জন্য পেট নিয়ে থাকেন, এখানে রয়েছে [StringBuilder](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-6.0)-এর উপর গভীর তথ্য।
