---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:02.880481-06:00
description: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u0985\u09A5\u09AC\u09BE\
  \ \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u09B9\u09BF\
  \u09B8\u09BE\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\
  \u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09DF\
  \u09C7\u09B0 \u09AA\u09DF\u09C7\u09A8\u09CD\u099F \u09A5\u09C7\u0995\u09C7 \u09A8\
  \u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u0982\u0996\u09CD\u09AF\u0995\
  \ \u09A6\u09BF\u09A8, \u09AE\u09BE\u09B8, \u0985\u09A5\u09AC\u09BE \u09AC\u099B\u09B0\
  \ \u09AA\u09C2\u09B0\u09CD\u09AC\u09C7 \u09AC\u09BE \u09AA\u09B0\u09C7 \u098F\u0995\
  \u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u0996\u09C1\u0981\u099C\u09C7 \u09AA\
  \u09BE\u0993\u09AF\u09BC\u09BE\u0964\u2026"
lastmod: '2024-03-17T18:47:44.098436-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u0985\u09A5\u09AC\u09BE \u0985\
  \u09A4\u09C0\u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u09B9\u09BF\u09B8\
  \u09BE\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09DF\u09C7\
  \u09B0 \u09AA\u09DF\u09C7\u09A8\u09CD\u099F \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\
  \u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u0982\u0996\u09CD\u09AF\u0995\
  \ \u09A6\u09BF\u09A8, \u09AE\u09BE\u09B8, \u0985\u09A5\u09AC\u09BE \u09AC\u099B\u09B0\
  \ \u09AA\u09C2\u09B0\u09CD\u09AC\u09C7 \u09AC\u09BE \u09AA\u09B0\u09C7 \u098F\u0995\
  \u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u0996\u09C1\u0981\u099C\u09C7 \u09AA\
  \u09BE\u0993\u09AF\u09BC\u09BE\u0964\u2026"
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

ভবিষ্যত অথবা অতীতের তারিখ হিসাব করা মানে একটি নির্দিষ্ট সময়ের পয়েন্ট থেকে নির্দিষ্ট সংখ্যক দিন, মাস, অথবা বছর পূর্বে বা পরে একটি তারিখ খুঁজে পাওয়া। প্রোগ্রামাররা মেয়াদ উত্তীর্ণের তারিখ, সূচি নির্ধারণ, অথবা ঘটনাবলীর মধ্যে সময় পার হওয়ার হিসাব জানার মত কাজের জন্য এটি করে থাকেন।

## কিভাবে:

Haskell তারিখ নিয়ে কাজ করার জন্য `time` এর মত লাইব্রেরিগুলি ব্যবহার করে। এখানে তারিখে দিন বা মাস যোগ করা বা তাদের বিয়োগ করে অতীতের একটি তারিখ খুঁজে পাওয়ার উপায় দেখানো হলো।

```Haskell
import Data.Time

-- বর্তমান তারিখে দিন যোগ করা
addDaysToCurrent :: Integer -> IO Day
addDaysToCurrent n = do
  today <- getCurrentTime
  timezone <- getCurrentTimeZone
  let localToday = utcToLocalTime timezone today
  return $ addDays n (localDay localToday)

-- ব্যবহার: addDaysToCurrent 10 বর্তমান তারিখে ১০ দিন যোগ করা

-- দিন যোগ অথবা বিয়োগ করে ভবিষ্যত বা অতীতের তারিখ গণনা করা
calculateDate :: Day -> Integer -> Day
calculateDate start n = addDays n start

-- ব্যবহারের উদাহরণ:
-- let futureDate = calculateDate (fromGregorian 2023 1 1) 90

-- মাস এবং বছর নিয়ে কাজ করার জন্য, আমরা `addGregorianMonthsClip` এবং `addGregorianYearsClip` ব্যবহার করি
calculateDateMonths :: Day -> Integer -> Day
calculateDateMonths start n = addGregorianMonthsClip n start

-- ব্যবহার:
-- let futureMonth = calculateDateMonths (fromGregorian 2023 1 1) 2

-- একটি তারিখ YYYY-MM-DD ফরম্যাটে প্রদর্শন
printFormattedDate :: Day -> IO ()
printFormattedDate date = putStrLn $ formatTime defaultTimeLocale "%F" date

-- ব্যবহার:
-- printFormattedDate futureDate
```

## গভীরে প্রবেশ

Haskell-এ, তারিখ গণনার জন্য আমরা প্রায়ই `time` লাইব্রেরিটি ব্যবহার করে থাকি। এই লাইব্রেরিটি DateTime অ্যারিথমেটিক, পার্সিং, এবং ফর্ম্যাটিং এর জন্য টাইপ এবং ফাংশন প্রদান করে। ঐতিহাসিকভাবে, মানুষ আগে ম্যানুয়ালি তারিখ সামঞ্জস্য করত, কিন্তু `time` এর মত লাইব্রেরিগুলি ক্যালেন্ডারের অসঙ্গতিগুলি (যেমন অধিবর্ষ) সামলায়।

`time` এর বিকল্পগুলির মধ্যে `Data.Time.Calendar.OrdinalDate` এবং `Data.Time.Clock.POSIX` রয়েছে, যা উপলব্ধি সংখ্যা অথবা টাইমস্ট্যাম্পের সাথে কাজ করার মত বিভিন্ন প্রয়োজনের জন্য।

বাস্তবায়নে, তারিখ গণনা আশ্চর্যজনকভাবে জটিল। `time` এর মত ফাংশনগুলি, যেমন `addGregorianMonthsClip` এর মাধ্যমে, মেলানো তারিখটি বৈধ হওয়া নিশ্চিত করা হয়। উদাহরণস্বরূপ, জানুয়ারি ৩১ তারিখে এক মাস যোগ করলে, এটি ফেব্রুয়ারির শেষ দিন (২৮ অথবা ২৯) পর্যন্ত "ক্লিপ" করবে, মার্চ ৩ তারিখে নয়।

## আরও দেখুন

- Haskell `time` লাইব্রেরি: http://hackage.haskell.org/package/time
- দ্য হাস্কেল স্কুল থেকে তারিখ এবং সময় নির্দেশিকা: https://school.haskellforall.com/#date-and-time
- ZonedTime এবং UTC ব্যাখ্যা: https://www.47deg.com/blog/dealing-with-time-in-haskell/
