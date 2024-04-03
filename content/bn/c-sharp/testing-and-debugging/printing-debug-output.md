---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:07:28.624994-06:00
description: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F\
  \ \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F\u09BF\u0982 \u09AF\u09BE\u09B0 \u0985\
  \u09B0\u09CD\u09A5 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0\
  \ \u0985\u09A7\u09C0\u09A8\u09C7 \u0995\u09BF \u0998\u099F\u099B\u09C7 \u09A4\u09BE\
  \ \u09AC\u09CB\u099D\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09DF\
  \u09CB\u099C\u09A8\u09C0\u09DF \u09A4\u09A5\u09CD\u09AF \u09AC\u09C7\u09B0 \u0995\
  \u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BE \u0995\u09B0\u09C7 \u09AD\u09C7\u09B0\u09BF\u09DF\
  \u09C7\u09AC\u09B2\u09C7\u09B0 \u09AE\u09BE\u09A8, \u0995\u09BE\u09B0\u09CD\u09AF\
  \u09A8\u09BF\u09B0\u09CD\u09AC\u09BE\u09B9\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.039935-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F\u09BF\u0982 \u09AF\u09BE\u09B0 \u0985\u09B0\
  \u09CD\u09A5 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u0985\
  \u09A7\u09C0\u09A8\u09C7 \u0995\u09BF \u0998\u099F\u099B\u09C7 \u09A4\u09BE \u09AC\
  \u09CB\u099D\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09DF\u09CB\
  \u099C\u09A8\u09C0\u09DF \u09A4\u09A5\u09CD\u09AF \u09AC\u09C7\u09B0 \u0995\u09B0\
  \u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u098F\u099F\u09BE \u0995\u09B0\u09C7 \u09AD\u09C7\u09B0\u09BF\u09DF\u09C7\
  \u09AC\u09B2\u09C7\u09B0 \u09AE\u09BE\u09A8, \u0995\u09BE\u09B0\u09CD\u09AF\u09A8\
  \u09BF\u09B0\u09CD\u09AC\u09BE\u09B9\u09C7\u09B0 \u09AA\u09CD\u09B0\u09AC\u09BE\u09B9\
  , \u098F\u09AC\u0982 \u09AC\u09BE\u0997 \u0996\u09C1\u0981\u099C\u09C7 \u09AC\u09C7\
  \u09B0 \u0995\u09B0\u09A4\u09C7\u2014\u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u09A7\
  \u09B0\u09A8\u09C7\u09B0 \u09A1\u09BF\u099C\u09BF\u099F\u09BE\u09B2 \u09AC\u09A8\
  \u09C7 \u09B0\u09C1\u099F\u09BF\u09B0 \u099F\u09C1\u0995\u09B0\u09CB\u09B0 \u09AA\
  \u09CD\u09B0\u09AE\u09BE\u09A3\u0964."
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কি এবং কেন?
ডিবাগ আউটপুট প্রিন্টিং যার অর্থ আপনার কোডের অধীনে কি ঘটছে তা বোঝার জন্য প্রয়োজনীয় তথ্য বের করা। প্রোগ্রামাররা এটা করে ভেরিয়েবলের মান, কার্যনির্বাহের প্রবাহ, এবং বাগ খুঁজে বের করতে—মানে একধরনের ডিজিটাল বনে রুটির টুকরোর প্রমাণ।

## কিভাবে:
সরল কাজ: আউটপুট কনসোলে প্রিন্ট করতে `Console.WriteLine()` ব্যবহার করুন। বিশেষ করে ডিবাগিং উদ্দেশ্যে, `Debug.WriteLine()` আপনার যাবতীয় গতি হতে পারে, যদি আপনার ব্যবহারের নির্দেশিকাগুলোতে `System.Diagnostics` থাকে। আপনি যদি UI অ্যাপ্লিকেশন লক্ষ্য করে থাকেন, তা হলে `Trace.WriteLine()` আপনার জন্য দরকারী টুল হতে পারে যেহেতু এটি শ্রোতাদের আউটপুট ধরার অনুমতি দেয়।

```C#
ব্যবহার করে System;
ব্যবহার করে System.Diagnostics;

পাবলিক ক্লাস DebugExample
{
    পাবলিক স্ট্যাটিক ভয়েড মেইন()
    {
        int magicNumber = 42;
        Console.WriteLine("হ্যালো, লোকজন! আসুন ডিবাগ করি।");
        Debug.WriteLine($"জাদুর সংখ্যা হচ্ছে: {magicNumber}");

        // ধরে নিন আমাদের এখানে একটি শর্তাবলী আছে
        Trace.WriteLine("আমরা ম্যাট্রিক্সের মধ্যে আছি!");
    }
}
```

কনসোল আউটপুট দেখাবে:
```
হ্যালো, লোকজন! আসুন ডিবাগ করি।
```

ডিবাগ আউটপুট, আপনার IDE এর ডিবাগ আউটপুট জানালা বা শ্রোতারা দেখতে পাবেন:
```
জাদুর সংখ্যা হচ্ছে: 42
আমরা ম্যাট্রিক্সের মধ্যে আছি!
```

## গভীর দিকে
চলুন সময়ে ভ্রমণ করি। যখন C# নতুন ছিল, লোকেরা মেসেজ বক্সের সাথে ডিবাগ করত—কল্পনা করুন শত শত বার 'ওকে' ক্লিক করা। কিন্তু টুলস বিকাশ লাভ করে। 'Console.WriteLine()' পদ্ধতি একটি বিশ্বস্ত, দ্রুত উপায় আউটপুট প্রিন্ট করার জন্য, যা কনসোল অ্যাপগুলিতে সেরা ব্যবহৃত হয়। তবে, যখন আপনি কনসোল অ্যাপগুলি থেকে চলে এসেছেন এবং উইন্ডোজ ফর্ম বা WPF অ্যাপগুলি তৈরিতে উন্নতি করেছেন, `System.Diagnostics` নেমস্পেস থেকে 'Debug.WriteLine()' এবং 'Trace.WriteLine()' আরও আকর্ষণীয় হয়ে ওঠে।

'Debug.Writeline()' শুধুমাত্র ডিবাগ মোডে আউটপুট দেয়; রিলিজ মোডে এটি নীরব থাকে। এই আচরণ এটিকে সাময়িক ডিবাগ প্রিন্টগুলির জন্য চমত্�্ করে তোলে যেগুলি নিয়ে আপনার পরে মুছে ফেলার চিন্তা করতে হবে না। অন্যদিকে, 'Trace.WriteLine()' ডিবাগ এবং রিলিজ বিল্ডগুলির জন্য সক্রিয় করা যায়, যা ডিপ্লয়মেন্ট পরবর্তী সমস্যাগুলি ট্রেস করতে সহায়ক হতে পারে।

মনে রাখা দরকার, `Debug` এবং `Trace` কলগুলি আপনার কোড জুড়ে ছড়িয়ে দেওয়া যায় এবং আপনি শ্রোতাগুলি ব্যবহার করে তাদের আউটপুট নিয়ন্ত্রণ ক�্ পারেন, প্রতিবার আউটপুট যেখানে যায় তা পরিবর্তন করার সময় পুনরায় কম্পাইল করার প্রয়োজন ছাড়াই। দারুণ, তাই না?

## আরও দেখুন
আরও হাসি এবং জ্ঞানের টুকরো পেতে, এই লিঙ্কগুলিতে দেখুন:
- মাইক্রোসফটের অফিসিয়াল ডকুমেন্টেশন `Debug` এ: [Debug Class (System.Diagnostics)](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug)
- মাইক্রোসফটের অফিসিয়াল ডকুমেন্টেশন `Trace` এ: [Trace Class (System.Diagnostics)](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.trace)
- শ্রোতাগুলি এবং ট্রেস সোর্সগুলির গভীর গবেষণা: [Trace Listeners](https://docs.microsoft.com/en-us/dotnet/framework/debug-trace-profile/trace-listeners)
