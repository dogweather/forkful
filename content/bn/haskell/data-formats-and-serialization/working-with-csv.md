---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:15.121937-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Haskell \u098F, CSV \u09AB\u09BE\
  \u0987\u09B2\u0997\u09C1\u09B2\u09BF \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\
  \u099C \u0995\u09B0\u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7 `cassava`\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7, \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u099C\
  \u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC\
  \ \u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF\u0997\u09C1\u09B2\u09BF\u09B0 \u0985\u09A8\u09CD\u09AF\u09A4\u09AE\u0964\
  \ \u09A8\u09BF\u099A\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.107829-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u098F, CSV \u09AB\u09BE\u0987\u09B2\u0997\u09C1\u09B2\u09BF \u09A8\
  \u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AF\u09C7\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7 `cassava` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7, \u098F\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC\
  \ \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF\u09B0 \u0985\
  \u09A8\u09CD\u09AF\u09A4\u09AE\u0964 \u09A8\u09BF\u099A\u09C7 `cassava` \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 CSV \u09AB\u09BE\u0987\u09B2 \u09A5\
  \u09C7\u0995\u09C7 \u09AA\u09A1\u09BC\u09BE \u098F\u09AC\u0982 CSV \u09AB\u09BE\u0987\
  \u09B2\u09C7 \u09B2\u09C7\u0996\u09BE\u09B0 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0964\n\n**1."
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কিভাবে:
Haskell এ, CSV ফাইলগুলি নিয়ে কাজ করা যেতে পারে `cassava` লাইব্রেরি ব্যবহার করে, এর জন্য জনপ্রিয় তৃতীয় পক্ষের লাইব্রেরিগুলির অন্যতম। নিচে `cassava` ব্যবহার করে CSV ফাইল থেকে পড়া এবং CSV ফাইলে লেখার উদাহরণ দেওয়া হল।

**1. একটি CSV ফাইল পড়া:**

প্রথমে, নিশ্চিত করুন যে আপনার প্রজেক্টের cabal ফাইলে `cassava` ইন্সটল করা আছে অথবা Stack ব্যবহার করে ইন্সটল করুন।

একটি CSV ফাইল পড়ে প্রতিটি রেকর্ড প্রিন্ট করার একটি সাধারণ উদাহরণ নিচে দেওয়া হল। ধারণা করা হলো CSV ফাইলে দুটি কলাম আছে: নাম এবং বয়স।

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "people.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, age) ->
            putStrLn $ name ++ " is " ++ show (age :: Int) ++ " বছর বয়সী।"
```

ধরা যাক `people.csv` এ আছে:
```
John,30
Jane,25
```
আউটপুট হবে:
```
John হলেন 30 বছর বয়সী।
Jane হলেন 25 বছর বয়সী।
```

**2. একটি CSV ফাইল লেখা:**

একটি CSV ফাইল তৈরি করতে, আপনি `cassava` থেকে `encode` ফাংশন ব্যবহার করতে পারেন।

এখানে রেকর্ডের একটি তালিকা কিভাবে CSV ফাইলে লেখা যায় তা দেখানো হলো:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

এই প্রোগ্রামটি চালানোর পর, `output.csv` এ থাকবেঃ

```
John,30
Jane,25
```

Haskell ব্যবহার করে CSV ফাইলগুলির সাথে কাজ করার এই সংক্ষিপ্ত পরিচিতি `cassava` লাইব্রেরি ব্যবহার করে CSV ফাইল থেকে পড়া এবং লেখা দেখাচ্ছে, যা ভাষাটি নতুনদের জন্য ডেটা ম্যানিপুলেশন কাজগুলি আরও সহজপ্রাপ্য করে তোলে।
