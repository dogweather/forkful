---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:26.732180-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09DC \u09B9\u09BE\
  \u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7 \u09AA\u09B0\u09BF\u09A3\
  \u09A4 \u0995\u09B0\u09BE\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB \u09A6\
  \u09C7\u0993\u09AF\u09BC\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u099F\u09BF\
  \u09B0 \u09AA\u09CD\u09B0\u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0\u099F\u09BF\
  \ \u0985\u09AD\u09CD\u09B0 \u09AC\u09DC \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\
  \u09CD\u09B7\u09B0\u09C7 (uppercase) \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\
  \u09BE, \u09AF\u0996\u09A8\u0995\u09BF \u09AC\u09BE\u0995\u09BF\u0997\u09C1\u09B2\
  \u09BF \u099B\u09CB\u099F \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\
  \u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.931116-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09DC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4\
  \ \u0995\u09B0\u09BE\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB \u09A6\u09C7\
  \u0993\u09AF\u09BC\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u099F\u09BF\u09B0\
  \ \u09AA\u09CD\u09B0\u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0\u099F\u09BF \u0985\
  \u09AD\u09CD\u09B0 \u09AC\u09DC \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\
  \u09B7\u09B0\u09C7 (uppercase) \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09BE\
  , \u09AF\u0996\u09A8\u0995\u09BF \u09AC\u09BE\u0995\u09BF\u0997\u09C1\u09B2\u09BF\
  \ \u099B\u09CB\u099F \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\
  \u09C7\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
weight: 2
---

## কি এবং কেন?

স্ট্রিং বড় হাতের অক্ষরে পরিণত করার মানে হলো দেওয়া স্ট্রিংটির প্রথম অক্ষরটি অভ্র বড় হাতের অক্ষরে (uppercase) পরিণত করা, যখনকি বাকিগুলি ছোট হাতের অক্ষরে (lowercase) রাখা, প্রায়শই মানকীকরণ বা পাঠযোগ্যতার কারণে। প্রোগ্রামাররা প্রায়ই এই কাজটি করে থাকেন ডাটা সুসঙ্গতভাবে উপস্থাপন নিশ্চিত করতে, বিশেষ করে ব্যবহারকারীর ইন্টারফেইস বা ব্যবহারকারীর ইনপুট প্রসেস এবং প্রদর্শনের সময়।

## কিভাবে:

এলমে, নির্দিষ্টভাবে স্ট্রিং বড় হাতের অক্ষরে পরিণত করার জন্য কোনো বিল্ট-ইন ফাংশন নেই। তবে, `String` মডিউলের বিল্ট-ইন ফাংশন যেমন `toUpper`, `toLower`, `left`, এবং `dropLeft` ব্যবহার করে এই কাজ সহজেই করা যায়।

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- উদাহরণ ব্যবহার
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- আউটপুট: "Hello World"
```

অধিক জটিল পরিস্থিতির জন্য বা যদি আপনি সরাসরি স্ট্রিং বড় হাতের অক্ষরে পরিণত করার মতো একটি লাইব্রেরি ব্যবহার করতে চান, তবে আপনি `elm-community/string-extra` মতো তৃতীয়-পক্ষের প্যাকেজ বিবেচনা করতে পারেন। তবে, আমার সর্বশেষ আপডেট অনুযায়ী, এলমের ইকোসিস্টেম ভাষা এবং প্রজেক্টগুলি ঝরঝরে রাখার জন্য এই ধরনের কাজ বিল্ট-ইন ফাংশন ব্যবহার করে সামাল দেওয়ার উৎসাহ দেয়।

```elm
import String.Extra as StringExtra

-- যদি কোনো তৃতীয়-পক্ষের লাইব্রেরিতে `capitalize` ফাংশন থাকে
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- ধারণাধীন লাইব্রেরি ফাংশনের সাথে উদাহরণ ব্যবহার
main =
    "this is elm" |> capitalizeWithLibrary
    -- ধারণাধীন আউটপুট: "This is elm"
```

যদি আপনি স্ট্যান্ডার্ড লাইব্রেরির বাইরে অতিরিক্ত কার্যকারিতা খুঁজছেন, তবে স্ট্রিং ম্যানিপুলেশনের জন্য সর্বশেষ এবং সর্বাধিক প্রাধান্যের লাইব্রেরিগুলির জন্য এলম প্যাকেজ রিপোজিটরি সবসময় চেক করুন।
