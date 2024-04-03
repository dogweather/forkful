---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:48.722250-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B9\u09CD\u09AF\u09BE\u09B8\
  \u0995\u09C7\u09B2\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09B8\u0982\
  \u09AF\u09C1\u0995\u09CD\u09A4\u09BF\u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\
  \u09AF\u09BC\u09BE `(++)` \u0985\u09AA\u09BE\u09B0\u09C7\u099F\u09B0\u09C7\u09B0\
  \ \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AC\u09C7\u09B6 \u09B8\u09B9\u099C\
  ."
lastmod: '2024-03-17T18:47:44.073486-06:00'
model: gpt-4-0125-preview
summary: "\u09B9\u09CD\u09AF\u09BE\u09B8\u0995\u09C7\u09B2\u09C7 \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u09B8\u0982\u09AF\u09C1\u0995\u09CD\u09A4\u09BF\u09B0\
  \ \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE `(++)` \u0985\u09AA\
  \u09BE\u09B0\u09C7\u099F\u09B0\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\
  \ \u09AC\u09C7\u09B6 \u09B8\u09B9\u099C."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
weight: 3
---

## কিভাবে:
হ্যাসকেলে স্ট্রিং সংযুক্তির প্রক্রিয়া `(++)` অপারেটরের মাধ্যমে বেশ সহজ:

```Haskell
main :: IO ()
main = do
  let hello = "Hello"
  let world = "World!"
  
  -- (++) অপারেটর ব্যবহার করে
  putStrLn $ hello ++ " " ++ world
  
  -- উদাহরণ আউটপুট: "Hello World!"
```

কিন্তু সেখানে থেমে থাকা কেন? তালিকা জড়িত হলে `Data.List` থেকে `concat` এবং `intercalate` নামের ফাংশনগুলি আপনার জন্য আছে:

```Haskell
import Data.List (intercalate, concat)

main :: IO ()
main = do
  let wordsList = ["Haskell", "is", "cool"]
  
  -- স্ট্রিংস এর একটি তালিকা সংযুক্তি
  putStrLn $ concat wordsList
  -- উদাহরণ আউটপুট: "Haskelliscool"

  -- একটি বিভাজক দিয়ে স্ট্রিংগুলি অন্তর্ভুক্তি
  putStrLn $ intercalate " " wordsList
  -- উদাহরণ আউটপুট: "Haskell is cool"
```

## গভীর ডুব
আগে, হ্যাসকেলের `++` অপারেটর ML এর মতো ভাষাগুলিতে অনুরূপ অপারেশনগুলি থেকে অনুপ্রাণিত ছিল। এটি একটি ক্লাসিক, কিন্তু সবসময় সবচেয়ে দক্ষ নয়, বিশেষ করে বড় স্ট্রিংগুলি বা বিশাল সংযুক্তির কাজের জন্য। `++` এর প্রতিটি ব্যবহার একটি নতুন তালিকা তৈরি করে, মানে যদি আপনি বিগ ডেটা নিয়ে কাজ করছেন, আপনার একটি বেশি দক্ষ পদ্ধতির প্রয়োজন হতে পারে।

বিকল্প? অবশ্যই আছে। `Data.Text.Lazy.Builder` থেকে `Builder` টাইপ বড় টেক্সট ম্যানিপুলেশনের জন্য ভালো অপ্টিমাইজ করা যেতে পারে। এটি চাঙ্কে কাজ করে টেক্সট আরও অর্থনৈতিকভাবে নির্মাণ করে, সারাক্ষণ সম্পূর্ণ কাজ কপি করার প্রয়োজন হ্রাস করে।

উদাহরণস্বরূপ, `Builder` দিয়ে কাজ করা:

```Haskell
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import Data.Text.Lazy.IO as T

main :: IO ()
main = do
  let builder1 = fromString "Haskell"
  let builder2 = fromString " "
  let builder3 = fromString "is"
  let builder4 = fromString " "
  let builder5 = fromString "neat!"

  let result = mconcat [builder1, builder2, builder3, builder4, builder5]
  -- বিল্ডারগুলিকে মার্জ করতে mconcat ব্যবহার করা হচ্ছে

  T.putStrLn $ toLazyText result
  -- উদাহরণ আউটপুট: "Haskell is neat!"
```

কেন `Builder` বা `concat` এর কাছে যেতে হবে? তারা বড় ডেটা নিয়ে চিন্তা ছাড়াই কাজ করে, আপনাকে পারফরম্যান্সের সমস্যায় ডুবে না যেতে দিয়ে পাঠ্যকে সংযোজন করতে দেয়।

## আরও দেখুন
- [Performance/Strings](https://wiki.haskell.org/Performance/Strings) এ হ্যাসকেল উইকি পারফরম্যান্স বিবেচনাগুলি গভীর দিকে ডুব দিতে।
- হ্যাসকেলে ইউনিকোড টেক্সট নিয়ে কাজ করার জন্য `Data.Text` [প্যাকেজ ডকুমেন্টেশন](https://hackage.haskell.org/package/text)।
- [হ্যাসকেল ভাষার ওয়েবসাইট](https://www.haskell.org/) হ্যাসকেল সম্পর্কিত সব খবর আপ-টু-ডেট রাখার জন্য।
