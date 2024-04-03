---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:40:02.124593-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B9\u09BE\u09B8\u09CD\u0995\
  \u09C7\u09B2\u09C7\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\
  \u09B0\u09CD\u09A1 \u09AA\u09CD\u09B0\u09BF\u09B2\u09BF\u0989\u09A1 `System.IO`\
  \ \u09AE\u09A1\u09BF\u0989\u09B2 \u09A5\u09C7\u0995\u09C7 `writeFile` \u098F\u09AC\
  \u0982 `appendFile` \u09AB\u09BE\u0982\u09B6\u09A8\u09C7\u09B0 \u09AE\u09BE\u09A7\
  \u09CD\u09AF\u09AE\u09C7 \u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\u09BF\u0996\u09BE\
  \ \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A8\u09A4\u09C1\u09A8 \u09AB\u09BE\u0987\
  \u09B2\u2026"
lastmod: '2024-03-17T18:47:44.103824-06:00'
model: gpt-4-0125-preview
summary: "\u09B9\u09BE\u09B8\u09CD\u0995\u09C7\u09B2\u09C7\u09B0 \u09B8\u09CD\u099F\
  \u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09AA\u09CD\u09B0\u09BF\
  \u09B2\u09BF\u0989\u09A1 `System.IO` \u09AE\u09A1\u09BF\u0989\u09B2 \u09A5\u09C7\
  \u0995\u09C7 `writeFile` \u098F\u09AC\u0982 `appendFile` \u09AB\u09BE\u0982\u09B6\
  \u09A8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AB\u09BE\u0987\
  \u09B2\u09C7 \u09B2\u09BF\u0996\u09BE \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u0995\
  \u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A8\
  \u09A4\u09C1\u09A8 \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF (\u0985\u09A5\
  \u09AC\u09BE \u098F\u0995\u099F\u09BF \u09AC\u09BF\u09A6\u09CD\u09AF\u09AE\u09BE\
  \u09A8 \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u0989\u09AA\u09B0 \u09B2\u09BF\u0996\
  \u09A8) \u098F\u09AC\u0982 \u09A4\u09BE\u09B0\u09AA\u09B0 \u098F\u0995\u099F\u09BF\
  \ \u09AB\u09BE\u0987\u09B2\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AF\u09CB\
  \u0997 \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\
  \u0995 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\u0964."
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কিভাবে:
হাস্কেলের স্ট্যান্ডার্ড প্রিলিউড `System.IO` মডিউল থেকে `writeFile` এবং `appendFile` ফাংশনের মাধ্যমে ফাইলে লিখা সমর্থন করে। এখানে একটি নতুন ফাইল তৈরি (অথবা একটি বিদ্যমান ফাইলের উপর লিখন) এবং তারপর একটি ফাইলে টেক্সট যোগ করার একটি মৌলিক উদাহরণ।

```haskell
import System.IO

-- একটি ফাইলে লিখন, যদি এটি বিদ্যমান থাকে তবে এর উপরে লিখন
main :: IO ()
main = do
  writeFile "example.txt" "This is line one.\n"
  appendFile "example.txt" "This is line two.\n"
```

যখন আপনি এই প্রোগ্রাম চালাবেন, এটি `example.txt` তৈরি করবে (অথবা পরিস্কার করবে) এবং "This is line one." এর পরের লাইনে "This is line two." লিখবে।

আরও উন্নত ফাইল হ্যান্ডলিং এর জন্য, হাস্কেল প্রোগ্রামাররা প্রায়শই `text` প্যাকেজের দিকে ঝুঁকেন যা দক্ষতার সাথে স্ট্রিং প্রসেসিং এবং `bytestring` প্যাকেজের জন্য বাইনারি ডেটা হ্যান্ডলিং সমর্থন করে। এখানে `text` প্যাকেজ ব্যবহারের উপায়:

প্রথমে, আপনার প্রকল্পের ডিপেন্ডেন্সিতে `text` যুক্ত করতে হবে। তারপর, আপনি এটি নিম্নরূপ ব্যবহার করতে পারেনঃ

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- text প্যাকেজ ব্যবহার করে ফাইলে লিখন
main :: IO ()
main = do
  let content = T.pack "Using the text package for better performance.\n"
  TIO.writeFile "textExample.txt" content
  TIO.appendFile "textExample.txt" $ T.pack "Appending line two.\n"
```

এই স্নিপেটে, `T.pack` একটি সাধারণ `String` কে `Text` টাইপে রূপান্তর করে, যা আরও দক্ষ। `TIO.writeFile` এবং `TIO.appendFile` যথাক্রমে ফাইলে লিখন এবং যোগ করার `text` সমতুল্য।

এই কোড চালানো হলে দুটি লাইনের টেক্সট সহ একটি ফাইল নামে `textExample.txt` তৈরি করবে, যা ইউনিকোড টেক্সট হ্যান্ডলিংয়ে আরও ভাল পারফরমেন্স এবং সক্ষমতা দেখানোর জন্য উন্নত `text` লাইব্রেরি ব্যবহার করে তৈরী ও যোগ করার সামর্থ্য দেমনস্ট্রেট করে।
