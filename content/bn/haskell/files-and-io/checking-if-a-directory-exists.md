---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:21.624109-06:00
description: "\u098F\u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\
  \u09BF \u0986\u099B\u09C7 \u0995\u09BF\u09A8\u09BE \u09A4\u09BE \u09AF\u09BE\u099A\
  \u09BE\u0987 \u0995\u09B0\u09BE \u0985\u09A8\u09C7\u0995 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u0995\u09BE\u099C\u09C7\u09B0 \u098F\
  \u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u0985\u09AA\u09BE\u09B0\u09C7\
  \u09B6\u09A8, \u09AF\u09BE\u09A4\u09C7 \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\
  \u09B0\u09BF\u09B0 \u0989\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u0985\u09A5\
  \u09AC\u09BE \u0985\u09A8\u09C1\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u09AD\
  \u09BF\u09A4\u09CD\u09A4\u09BF\u0995 \u09B6\u09B0\u09CD\u09A4\u09BE\u09A7\u09C0\u09A8\
  \ \u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09B2\u09BE\u09AA\u2026"
lastmod: '2024-03-17T18:47:44.099480-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\
  \u09BF \u0986\u099B\u09C7 \u0995\u09BF\u09A8\u09BE \u09A4\u09BE \u09AF\u09BE\u099A\
  \u09BE\u0987 \u0995\u09B0\u09BE \u0985\u09A8\u09C7\u0995 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u0995\u09BE\u099C\u09C7\u09B0 \u098F\
  \u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u0985\u09AA\u09BE\u09B0\u09C7\
  \u09B6\u09A8, \u09AF\u09BE\u09A4\u09C7 \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\
  \u09B0\u09BF\u09B0 \u0989\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u0985\u09A5\
  \u09AC\u09BE \u0985\u09A8\u09C1\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u09AD\
  \u09BF\u09A4\u09CD\u09A4\u09BF\u0995 \u09B6\u09B0\u09CD\u09A4\u09BE\u09A7\u09C0\u09A8\
  \ \u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09B2\u09BE\u09AA\u2026"
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
একটি ডিরেক্টরি আছে কিনা তা যাচাই করা অনেক প্রোগ্রামিং কাজের একটি মৌলিক অপারেশন, যাতে ডিরেক্টরির উপস্থিতি অথবা অনুপস্থিতি ভিত্তিক শর্তাধীন ক্রিয়াকলাপ সম্ভব হয়। এটি ফাইল পরিচালনা, স্বয়ংক্রিয় স্ক্রিপ্ট এবং সফটওয়্যারের প্রাথমিক সেটআপ কালে আবশ্যিক ডিরেক্টরিগুলি স্থাপনের নিশ্চয়তা প্রদান অথবা ডিরেক্টরি পুনরায় তৈরির এড়িয়ে চলার জন্য অপরিহার্য।

## কিভাবে:
হাসকেল, এর বেস লাইব্রেরির মাধ্যমে, ডিরেক্টরির অস্তিত্ব যাচাই করার জন্য সোজাসাপটা উপায় সরবরাহ করে, মূলত `System.Directory` মডিউল ব্যবহার করে। চলুন একটি বেসিক উদাহরণের দিকে নজর দেই:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/path/to/your/directory"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "Does the directory exist? " ++ show exists
```

নির্ভর করে ডিরেক্টরির উপস্থিতি না উপস্থিতিতে, নমুনা আউটপুট:

```
Does the directory exist? True
```
অথবা:
```
Does the directory exist? False
```

আরও জটিল পরিস্থিতি অথবা অতিরিক্ত ফাংশনালিটির জন্য, আপনি ফাইল পাথগুলি আরও অস্পষ্টভাবে পরিচালনা এবং ম্যানিপুলেশনের জন্য `filepath` এর মতো জনপ্রিয় থার্ড-পার্টি লাইব্রেরি বিবেচনা করতে পারেন। তবে, কেবল একটি ডিরেক্টরির অস্তিত্ব যাচাই করার উদ্দেশ্যে, বেস লাইব্রেরির `System.Directory` যথেষ্ট এবং দক্ষ।

মনে রাখবেন, ফাইল সিস্টেম নিয়ে কাজ বিভিন্ন প্ল্যাটফর্মে বিভিন্ন হতে পারে, এবং হাসকেলের পদ্ধতি এই পার্থক্যগুলির কিছুটা অস্পষ্ট করার লক্ষ্যে থাকে। প্রত্যাশিত আচরণ নিশ্চিত করতে লক্ষ্যভুক্ত সিস্টেমে আপনার ফাইল অপারেশনগুলি সর্বদা পরীক্ষা করুন।
