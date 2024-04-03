---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:12.517012-06:00
description: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\
  \u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\
  \u09CB\u09A1\u09C7 \u09A1\u09C7\u099F\u09BE \u09B6\u09CB\u09B7\u09A3 \u0995\u09B0\
  \u09BE, \u09AF\u09C7\u09AE\u09A8 \u09B8\u0995\u09BE\u09B2\u09C7 \u09AE\u09B8\u09CD\
  \u09A4\u09BF\u09B7\u09CD\u0995\u09C7 \u0995\u09AB\u09BF \u09A2\u09BE\u09B2\u09BE\
  \u0964 \u0986\u09AE\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09BF \u0995\u09BE\
  \u09B0\u09A3 \u098F\u09A4\u09C7 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u098F\u09AE\u09A8 \u09A4\u09A5\u09CD\
  \u09AF \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.102814-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\
  \u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\
  \u09CB\u09A1\u09C7 \u09A1\u09C7\u099F\u09BE \u09B6\u09CB\u09B7\u09A3 \u0995\u09B0\
  \u09BE, \u09AF\u09C7\u09AE\u09A8 \u09B8\u0995\u09BE\u09B2\u09C7 \u09AE\u09B8\u09CD\
  \u09A4\u09BF\u09B7\u09CD\u0995\u09C7 \u0995\u09AB\u09BF \u09A2\u09BE\u09B2\u09BE\
  \u0964 \u0986\u09AE\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09BF \u0995\u09BE\
  \u09B0\u09A3 \u098F\u09A4\u09C7 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u098F\u09AE\u09A8 \u09A4\u09A5\u09CD\
  \u09AF \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\
  \ \u09AF\u09BE \u0985\u09A8\u09CD\u09AF\u09A5\u09BE\u09AF\u09BC \u09A8\u09BE \u09AA\
  \u09C7\u09B2\u09C7 \u09A4\u09BE\u09B0\u09BE \u0995\u09BE\u099C \u0995\u09B0\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09AC\u09C7 \u09A8\u09BE, \u09AF\u09C7\u09AE\u09A8 \u09B8\
  \u09C7\u099F\u09BF\u0982\u09B8\u09CD\u200C, \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\
  \u09BF\u09AF\u09BC\u09BE\u099C\u09BE\u09A4 \u0995\u09B0\u09BE\u09B0 \u09A1\u09C7\
  \u099F\u09BE \u09AC\u09BE \u0995\u09BE\u09B0\u09CD\u09AF \u09B8\u09AE\u09CD\u09AA\
  \u09BE\u09A6\u09A8\u09C7\u09B0 \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6\u09A8\u09BE\
  \u0964."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কীভাবে:
হাসকেল দিয়ে কীভাবে টেক্সট ফাইল পড়তে হয় তা না ভাঙ্গাচোরা করে এখানে দেখানো হলো। আপনার প্রিয় সম্পাদক খুলুন, এবং আমরা কিছু কোড লিখি।

```Haskell
import System.IO

main = do
    -- ফাইলটি পড়ার মোডে খুলতে
    handle <- openFile "hello.txt" ReadMode
    -- ফাইলের বিষয়বস্তু পড়তে
    content <- hGetContents handle
    -- ফাইলের বিষয়বস্তু প্রিন্ট করতে
    putStrLn content
    -- ফাইল হ্যান্ডেল বন্ধ না করার কথা ভুলবেন না!
    hClose handle
```

এটি চালান, এবং যদি আপনার "hello.txt" মধ্যে "Hello, World!" থাকে, আপনি পাবেন:

```
Hello, World!
```

এখানে আরেকটি ছোট এবং ঝকঝকে উপায়, কম ঝক্কি নিয়ে একই কাজ:

```Haskell
-- 'readFile' খোলা এবং পড়া এক সঙ্গে করে
main = do
    content <- readFile "hello.txt"
    putStrLn content
```

আউটপুট এখনও,

```
Hello, World!
```

## গভীর ডুব
বহু আগে, প্রোগ্রামগুলি ছিল অসামাজিক প্রাণী, মূলত নিজেদের উত্পন্ন ডেটা প্রক্রিয়া করে। কিন্তু জটিলতা বাড়তে থাকে, এবং বাহ্যিক তথ্য আনার প্রয়োজনও বাড়তে থাকে, তাই ফাইল থেকে পড়া একটি মূল অংশ হয়ে ওঠে।

হাসকেলে বিভিন্ন উপায়ে ফাইল পড়া যায়। আমরা `openFile`, `hGetContents`, এবং `hClose` এর মাধ্যমে নিম্ন-স্তরে এটি করতে পারি অথবা `readFile` দিয়ে খেলতে পারি, যা সবকিছু গোছালো করে।

`readFile` হল অলস - এটি যখন প্রয়োজন হয় তখন বিষয়বস্তু পড়ে, যা বড় ফাইলের জন্য মেমোরি দক্ষ কিন্তু যদি ফাইলটি মাঝপথে পরিবর্তন হয় তবে এটি আশ্চর্যজনক হতে পারে। নিম্ন-স্তরের পদ্ধতিটি আরও নিয়ন্ত্রণ দেয়, যা এটিকে আরও প্রেডিক্টেবল কিন্তু একই সঙ্গে বাকপটু করে তোলে। বিশাল টেক্সটের জন্য, হাসকেলের `hGetLine` বা `conduit` এবং `pipes` এর মতো লাইব্রেরিগুলি মেমোরি এবং প্রক্রিয়াজাতকরণ আরও সূক্ষ্মভাবে পরিচালনা করার সুযোগ দেয়।

হাসকেলের মানক `IO` কার্যাবলী ফাইলগুলিকে মৌলিক OS পদ্ধতিগুলি ব্যবহার করে হ্যান্ডল করে। লাইব্রেরিগুলি এই অপারেশনগুলিকে বেশি ব্যবহারকারী-বান্ধব করে তোলে কিন্তু শেষ পর্যন্ত, তারা হাসকেলের `IO` মোনাডের উপর নির্মিত, যা নিশ্চিত করে যে কার্যাবলীগুলি সঠিক ক্রমে ঘটে।

## আরও দেখুন
- অফিশিয়াল হাসকেল ডকুমেন্টেশনের জন্য, [হাসকেলের ইনপুট এবং আউটপুট ডকুমেন্টেশন](https://www.haskell.org/tutorial/io.html) দেখুন।
- আরও জানার জন্য, [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/input-and-output)-এ জ্ঞানের এক কাপ উপভোগ করুন।
- ফাইল ম্যানেজমেন্টের গভীর বোঝাপড়া পেতে [Real World Haskell's take on IO](http://book.realworldhaskell.org/read/io.html)-এ গভীর ডুব দিন।
- বড় ফাইলগুলির জন্য স্ট্রিমিং লাইব্রেরিগুলি অন্বেষণ করতে [conduit](https://hackage.haskell.org/package/conduit) এবং [pipes](https://hackage.haskell.org/package/pipes) দেখুন।
