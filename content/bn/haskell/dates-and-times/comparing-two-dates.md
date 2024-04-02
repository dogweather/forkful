---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:44.379691-06:00
description: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\
  \u09B2\u09A8\u09BE \u0995\u09B0\u09BE\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\u099A\
  \u09CD\u099B\u09C7 \u098F\u0995\u099F\u09BF \u0995\u09CB\u09A8\u099F\u09BF \u0986\
  \u0997\u09C7, \u09AA\u09B0\u09C7 \u0985\u09A5\u09AC\u09BE \u09A4\u09BE\u09B0\u09BE\
  \ \u09AF\u09A6\u09BF \u098F\u0995\u0987 \u09AE\u09C1\u09B9\u09C2\u09B0\u09CD\u09A4\
  \u09C7 \u0985\u09AC\u09B8\u09CD\u09A5\u09BE\u09A8 \u0995\u09B0\u09C7 \u09A4\u09BE\
  \ \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0998\u099F\u09A8\u09BE\
  \u09AC\u09B2\u09C0 \u09B8\u09BE\u099C\u09BE\u09A8\u09CB, \u09B8\u09AE\u09DF \u0995\
  \u09BE\u09B2\u09C7\u09B0 \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF\u2026"
lastmod: '2024-03-17T18:47:44.097428-06:00'
model: gpt-4-0125-preview
summary: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\u099A\u09CD\
  \u099B\u09C7 \u098F\u0995\u099F\u09BF \u0995\u09CB\u09A8\u099F\u09BF \u0986\u0997\
  \u09C7, \u09AA\u09B0\u09C7 \u0985\u09A5\u09AC\u09BE \u09A4\u09BE\u09B0\u09BE \u09AF\
  \u09A6\u09BF \u098F\u0995\u0987 \u09AE\u09C1\u09B9\u09C2\u09B0\u09CD\u09A4\u09C7\
  \ \u0985\u09AC\u09B8\u09CD\u09A5\u09BE\u09A8 \u0995\u09B0\u09C7 \u09A4\u09BE \u09A8\
  \u09BF\u09B0\u09CD\u09A3\u09DF \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0998\u099F\u09A8\u09BE\u09AC\
  \u09B2\u09C0 \u09B8\u09BE\u099C\u09BE\u09A8\u09CB, \u09B8\u09AE\u09DF \u0995\u09BE\
  \u09B2\u09C7\u09B0 \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF\u2026"
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কি এবং কেন?

দুটি তারিখ তুলনা করার মানে হচ্ছে একটি কোনটি আগে, পরে অথবা তারা যদি একই মুহূর্তে অবস্থান করে তা নির্ণয় করা। প্রোগ্রামাররা ঘটনাবলী সাজানো, সময় কালের নির্ণয় করা, এবং সময়-নির্ভর যুক্তি পরিচালনা করার জন্য এটি করে।

## কিভাবে করবেন:

হাস্কেল, এর পবিত্রতার জন্য নীরবে পরিচিত, আপনাকে সঠিক লাইব্রেরী দিয়ে তারিখ-ভাষায় কথা বলতে হবে। আসুন `Data.Time` ব্যবহার করি।

```haskell
import Data.Time

-- দুটি তারিখ নির্ধারণ 
date1 :: UTCTime
date1 = UTCTime (fromGregorian 2023 4 1) (secondsToDiffTime 0)

date2 :: UTCTime
date2 = UTCTime (fromGregorian 2024 4 2) (secondsToDiffTime 3600)

-- তারিখগুলি তুলনা করা
compareDates :: UTCTime -> UTCTime -> Ordering
compareDates = compare

main :: IO ()
main = do
    print $ date1 `compareDates` date2 -- আউটপুট হবে LT
    print $ date2 `compareDates` date1 -- আউটপুট হবে GT
    print $ date1 `compareDates` date1 -- আউটপুট হবে EQ
```

সরল, তাই না? `LT` মানে হলো কম, `GT` মানে হলো বেশি, এবং `EQ` মানে হলো সমান।

## গভীর ডুব

দিনগুলিতে, হাস্কেলের সময় ব্যবস্থাপনা এত চমৎকার ছিল না। আমাদের বর্তমান সুবিধাগুলির জন্য আমরা `Data.Time` লাইব্রেরীর উন্নতির কাছে কৃতজ্ঞ। এটি আমাদের `UTCTime` প্রদান করে, একটি সুখীভাবে অস্পষ্ট মুহূর্ত।

বিকল্প? অবশ্যই। আপনি নির্দিষ্ট পরিস্থিতিতে `Data.Time.Calendar` এবং `Data.Time.Clock` উপযোগী পাবেন। পুরনো কোডে আটকে আছেন অথবা নস্টালজিক অনুভব করছেন এমন ব্যক্তিদের জন্য পুরনো `time` লাইব্রেরীও রয়েছে।

এখন, গুণগত বিষয়: হাস্কেলে তারিখ তুলনা করা `UTCTime` নির্ভর করে, যেটি একটি দিন (`Day`) এবং একটি সময় (`DiffTime` অথবা `NominalDiffTime`) জুড়ে দেয়। এটি `compare` ফাংশন যা বেশিরভাগ ভার বহন করে, `Ord` ক্লাসের একটি পরিষ্কার সদস্য, আমাদের `>, <, ==` এবং আরও অনেক কিছু ব্যবহার করতে দেয়। শুধু মনে রাখবেন হাস্কেল এর টাইপ নিরাপত্তা ভালোবাসে। নিশ্চিত করুন আপনি সর্বদা আপেলের সাথে আপেল, অথবা আমাদের ক্ষেত্রে, `UTCTime`-এর সাথে `UTCTime` তুলনা করছেন।

## আরও দেখুন

এইগুলি দিয়ে আরো গভীরে ডুব দিন অথবা সাহায্য পান:
- [`Data.Time` প্যাকেজ Hackage-এ](https://hackage.haskell.org/package/time-1.11/docs/Data-Time.html)
- [লার্ন ইউ আ হাস্কেল ফর গ্রেট গুড! – একটি সৌম্য পরিচয়ের জন্য](http://learnyouahaskell.com/)
- [স্ট্যাক ওভারফ্লো বাস্তব জগতের সমস্যা সমাধানের জন্য](https://stackoverflow.com/questions/tagged/haskell+time)
