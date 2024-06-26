---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:22:13.250192-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0986\u09B8\u09C1\u09A8 \u098F\
  \u0995\u099F\u09BF \u09AA\u09A5\u099A\u09B2\u09BE\u09DF \u09AF\u09BE\u0987 GHCi\
  \ \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7, Haskell \u098F\u09B0 \u0987\u09A8\u09CD\
  \u099F\u09BE\u09B0\u200C\u09CD\u09AF\u09BE\u0995\u099F\u09BF\u09AD \u09AA\u09B0\u09BF\
  \u09AC\u09C7\u09B6 \u09AF\u09BE \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\
  \u0995 \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09B9\u09BF\u09B8\u09C7\u09AC\
  \u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964\
  \ \u0986\u09AA\u09A8\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 Haskell \u0995\u09CB\u09A1\
  \ \u09A6\u09BF\u09AF\u09BC\u09C7 \u098F\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.088401-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B8\u09C1\u09A8 \u098F\u0995\u099F\u09BF \u09AA\u09A5\u099A\u09B2\
  \u09BE\u09DF \u09AF\u09BE\u0987 GHCi \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7, Haskell\
  \ \u098F\u09B0 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\u09AF\u09BE\u0995\
  \u099F\u09BF\u09AD \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6 \u09AF\u09BE \u098F\u0995\
  \u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\
  \u09B0 \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u0986\u09AA\u09A8\u09BF \u0986\u09AA\u09A8\
  \u09BE\u09B0 Haskell \u0995\u09CB\u09A1 \u09A6\u09BF\u09AF\u09BC\u09C7 \u098F\u099F\
  \u09BF \u099A\u09BE\u09B2\u09C1 \u0995\u09B0\u09C7\u09A8 \u098F\u09AC\u0982 \u0986\
  \u09B6\u09C7\u09AA\u09BE\u09B6\u09C7 \u09A8\u09BE\u09A1\u09BC\u09BE\u099A\u09BE\u09A1\
  \u09BC\u09BE \u0995\u09B0\u09BE \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09C7\u09A8\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u0989\u09A6\u09BE\
  \u09B9\u09B0\u09A3."
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
weight: 35
---

## কিভাবে:
আসুন একটি পথচলায় যাই GHCi এর সাথে, Haskell এর ইন্টার‌্যাকটিভ পরিবেশ যা একটি মৌলিক ডিবাগার হিসেবে কাজ করতে পারে। আপনি আপনার Haskell কোড দিয়ে এটি চালু করেন এবং আশেপাশে নাড়াচাড়া করা শুরু করেন। এখানে একটি উদাহরণ:

```Haskell
main :: IO ()
main = do
    putStrLn "হেই, তোমার নাম কি?"
    name <- getLine
    putStrLn $ "হ্যালো, " ++ name ++ "! চলো ডিবাগ করি।"
    let result = buggyFunction 5
    print result

buggyFunction :: Int -> Int
buggyFunction n = n * 2 -- ধরুন এখানে একটি বাগ রয়েছে
```

GHCi এর সাথে ডিবাগিং শুরু করতে:

```bash
$ ghci YourHaskellFile.hs
```

`buggyFunction` এ একটি ব্রেকপয়েন্ট সেট করুন:

```Haskell
Prelude> :break buggyFunction
```

আপনার প্রোগ্রাম চালান:

```Haskell
Prelude> :main
হেই, তোমার নাম কি?
```

আপনার প্রোগ্রাম `buggyFunction` এ থেমে যায়। এখন আপনি ভ্যারিয়েবল পরীক্ষা করতে, কোডের মধ্যে ধাপে ধাপে এগোতে এবং এক্সপ্রেশন মূল্যায়ন করতে পারেন।

## গভীর ডুব:
ঐতিহাসিক দিক দিয়ে, Haskell এর শুদ্ধ ফাংশন এবং শক্তিশালী টাইপিং এর কারণে মনে করা হতো যে ডিবাগার সরঞ্জামগুলি কম গুরুত্বপূর্ণ। বাস্তবিকতা আলাদা-জটিল প্রোগ্রামগুলি সর্বদা ভালো ডিবাগিং সরঞ্জাম থেকে উপকার পায়। GHCi মৌলিক ডিবাগিং কমান্ড প্রদান করে। তবে, একটি আরো ভিজ্যুয়াল অভিজ্ঞতা বা বৃহত্তর আকারের অ্যাপ্লিকেশনের জন্য, আপনি Visual Studio Code এর Haskell এক্সটেনশনগুলি বা IntelliJ এর Haskell প্লাগিন যেমন IDE গুলিতে ইন্টিগ্রেটেড ডিবাগারে খোঁজ করতে পারেন।

ডিবাগারের বিকল্প হিসেবে প্রিন্ট স্টেটমেন্টগুলি ব্যবহার করা অন্তর্ভুক্ত, যা "printf ডিবাগিং" নামে পরিচিত, অথবা Haskell এর শক্তিশালী টাইপ সিস্টেম ব্যবহার করে ভুল অবস্থাগুলিকে অপ্রতিনিধিত করা। তবে, মাঝে মাঝে কোডের মধ্যে ধাপে ধাপে এগোনোর জন্য এর কোনো বিকল্প নেই।

বাস্তবায়নের বিস্তারিত হিসেবে, Haskell এর ডিবাগার রানটাইম সিস্টেমের সাথে কাজ করে। এটি ব্রেকপয়েন্ট, ধাপ নির্বাহ, এবং ভ্যারিয়েবল পরীক্ষার মতো বিষয়গুলিকে সামলাতে পারে। তবে, Haskell এর আলস্যপ্রবণ মূল্যায়নের কারণে, জিনিসগুলি কিছুটা অ-স্বাভাবিক হতে পারে। Haskell প্রোগ্রাম ডিবাগ করা মানে অভিব্যক্তিগুলি কখন এবং কিভাবে মূল্যায়ন করা হচ্ছে তা নজরে রাখা।

## দেখুনও:
- [GHC ব্যবহারকারীর গাইড - ডিবাগার](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [IntelliJ Haskell প্লাগিন](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)
