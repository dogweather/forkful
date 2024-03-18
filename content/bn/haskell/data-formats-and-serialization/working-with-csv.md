---
title:                "CSV এর সাথে কাজ করা"
date:                  2024-03-17T18:28:15.121937-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?

CSVs (Comma-Separated Values) নিয়ে কাজ করা মানে টেক্সট-ভিত্তিক ফর্ম্যাটে ট্যাবুলার ডাটা সংরক্ষণ করা ফাইলগুলি পার্স করা এবং তৈরি করা। প্রোগ্রামাররা প্রায়শই এই কাজ সম্পন্ন করে থাকেন স্প্রেডশিট, ডাটাবেস থেকে ডেটা আমদানি অথবা রপ্তানি করার জন্য, অথবা বিভিন্ন প্রোগ্রামের মধ্যে ডেটা আদান-প্রদান সহজীকরণ করার জন্য।

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
