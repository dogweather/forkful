---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:19.629026-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: ."
lastmod: '2024-03-17T18:47:44.065228-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কীভাবে:
```haskell
import Data.List (isInfixOf)
import Data.Char (isSpace)

-- একটি স্ট্রিং থেকে প্যাটার্ন সরানোর সিম্পল ফাংশন
removePattern :: Eq a => [a] -> [a] -> [a]
removePattern [] _ = []
removePattern string@(x:xs) pattern
  | pattern `isInfixOf` string = removePattern (drop (length pattern) string) pattern
  | otherwise = x : removePattern xs pattern

-- একটি স্ট্রিং থেকে জায়গাগুলি ট্রিম করার জন্য প্রাকদর্শিত ফাংশনগুলি ব্যবহার করুন
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

main :: IO ()
main = do
  let text = "Haskell is super cool, super cool indeed."
  let cleanedText = removePattern text "super "
  putStrLn cleanedText  -- "Haskell is cool, cool indeed."
  putStrLn $ trim "   Trimmed whitespace   " -- "Trimmed whitespace"
```

## গভীরে গমন
Haskell-এর 'Data.List' এর মতো সমৃদ্ধ লাইব্রেরীসেট, স্ট্রিং যা মূলত তালিকাদের একটি বিশেষ ক্ষেত্রের জন্য বিভিন্ন টুলগুলি প্রদান করে। ঐতিহাসিকভাবে, Haskell-এর প্যাটার্ন ম্যাচিং হল ML এর মতো পুরানো ফাংশনাল ভাষাগুলি থেকে ধার করা একটি ধারণা।

Haskell-এ প্যাটার্ন ম্যাচিং এর বিভিন্ন পদ্ধতি রয়েছে। আমাদের সাধারণ `removePattern` `isInfixOf` ব্যবহার করে প্যাটার্ন চেক করে। জটিল প্যাটার্নের জন্য regex লাইব্রেরিগুলিও রয়েছে, তবে তারা নির্ভরতা যোগ করে এবং কখনও কখনও জিনিসগুলি অতিরিক্ত জটিল করে তোলে।

নির্ভরশীলতা প্রসঙ্গে, জায়গাগুলি ট্রিম করতে আপনি তৃতীয় পক্ষের একটি লাইব্রেরি ইমপোর্ট করতে পারেন, তবে আমাদের 'trim' ফাংশন নেটিভভাবে কাজ টি করে।

সর্বশেষে, কর্মক্ষমতা প্রসঙ্গে, Haskell-এ পুনরাবৃত্তিমূলক ফাংশনগুলি নিয়ে সবসময় সাবধান থাকুন; কম্পাইলার কর্তৃক সঠিকভাবে অনুকূলিত না হলে এগুলি অকার্যকর হতে পারে। থাঙ্কস জমা হতে পারে, যা স্থান ফাঁস ঘটাতে পারে। বৃহত্তর বা অধিক সংখ্যক স্ট্রিংগুলির ম্যানিপুলেশনের জন্য ভালো কর্মক্ষমতা পেতে, আপনি Haskell-এর `Text` মডিউল অন্বেষণ করতে পারেন।

## আরও দেখুন
- রিয়েল ওয়ার্ল্ড Haskell: http://book.realworldhaskell.org/
- Haskell `Data.List` ডকুমেন্টেশন: https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List.html
- Haskell উইকি কর্মক্ষমতা প্রসঙ্গে: https://wiki.haskell.org/Performance
