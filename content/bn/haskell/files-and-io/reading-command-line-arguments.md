---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:54.638794-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09AC\u09C7\u09A8\
  : ."
lastmod: '2024-03-17T18:47:44.100664-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

## কিভাবে করবেন:
```haskell
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hello, " ++ show args ++ "!")
```

এটি "world" আর্গুমেন্ট হিসেবে পাস করে চালান:

```bash
$ runhaskell yourprogram.hs world
Hello, ["world"]!
```

## গভীর ডাইভ
Haskell একটি নীট ভাষা, ৮০ এর দশকের মূলে, পবিত্রতা এবং স্ট্যাটিক টাইপিং এর দিকে ঝেঁকে। এটি কমান্ড লাইন আর্গুমেন্টস সামলানোর পথ ছিল প্রথম দিন থেকেই। অন্যান্য ভাষায়, এটা বেশ প্রক্রিয়ামূলক জিনিস হতে পারে, কিন্তু এখানে, আমরা IO মোনাদস এর রাজ্যে আছি ভয়ানক বাহ্যিক পৃথিবীর সাথে মোকাবেলার জন্য।

বিকল্প? জটিল জিনিসের জন্য আপনি `optparse-applicative` এর মতো লাইব্রেরিগুলি দিয়ে উদ্বেগ বিস্তার করতে পারেন, কিন্তু সহজ ক্ষেত্রের জন্য, `getArgs` কাজটা করে দেয়।

আড়ালে কি? `getArgs` হল একটি ফাংশন যে আপনার সিস্টেমে গভীরে ঢুকে, টার্মিনালে প্রোগ্রামের নামের পরে যা ছিল তা অনুসন্ধান করে, এবং আপনাকে একটি স্ট্রিংসের তালিকা হাতে দেয়। এটি Haskell এর বেস লাইব্রেরিতে বাস্তবায়িত, খাটুনি করার জন্য নিম্ন-স্তরের C ফাংশনের ওপর নির্ভর করে। চমৎকার, তাই না?

## আরো দেখুন
- `getArgs` দিয়ে গভীরে যাওয়া: [Hoogle on System.Environment](https://hoogle.haskell.org/?hoogle=System.Environment.getArgs)
- আর্গুমেন্ট পার্সিং এ উন্নতি: [optparse-applicative on Hackage](https://hackage.haskell.org/package/optparse-applicative)
