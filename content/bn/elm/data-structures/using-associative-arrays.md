---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:24:53.407701-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elm-\u098F, \u0986\u09AA\u09A8\
  \u09BF `Dict` \u09AE\u09A1\u09BF\u0989\u09B2\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\
  \u09AF\u09AE\u09C7 \u09A1\u09BF\u0995\u09B6\u09A8\u09BE\u09B0\u09BF\u0997\u09C1\u09B2\
  \u09BF\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09C7\u09A8\
  , \u09A4\u09BE\u0987 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u0989\
  \u09A6\u09BE\u09B9\u09B0\u09A3\u09C7 \u09A1\u09C1\u09AC \u09A6\u09BF\u09A4\u09C7\
  \ \u099A\u09B2\u09C1\u09A8."
lastmod: '2024-03-17T18:47:43.941230-06:00'
model: gpt-4-0125-preview
summary: "Elm-\u098F, \u0986\u09AA\u09A8\u09BF `Dict` \u09AE\u09A1\u09BF\u0989\u09B2\
  \u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09A1\u09BF\u0995\u09B6\
  \u09A8\u09BE\u09B0\u09BF\u0997\u09C1\u09B2\u09BF\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09C7\u09A8, \u09A4\u09BE\u0987 \u098F\u0995\u099F\
  \u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\u09C7\
  \ \u09A1\u09C1\u09AC \u09A6\u09BF\u09A4\u09C7 \u099A\u09B2\u09C1\u09A8."
title: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\u09CD\
  \u09AF\u09BE\u09B0\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0"
weight: 15
---

## কিভাবে:
Elm-এ, আপনি `Dict` মডিউলের মাধ্যমে ডিকশনারিগুলির সাথে কাজ করেন, তাই একটি দ্রুত উদাহরণে ডুব দিতে চলুন:

```Elm
import Dict exposing (Dict)

-- String কী এবং Int মানের সাথে একটি ডিকশনারি তৈরি করা
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- একটি মান যোগ করা অথবা আপডেট করা
updatedDict = Dict.insert "grape" 10 exampleDict

-- একটি মান পুনঃপ্রাপ্তি (লক্ষ্য করুন Maybe টাইপের, কারণ কী উপস্থিত নাও থাকতে পারে)
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- একটি কী-মান যুগল মুছে ফেলা
finalDict = Dict.remove "banana" updatedDict

-- একটি ডিকশনারিকে আবার তালিকায় রূপান্তর
dictToList = Dict.toList finalDict
```

`dictToList` প্রদর্শনের সময় নমুনা আউটপুট:

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

এটি ডিকশনারি তৈরি, আপডেট, অ্যাক্সেস এবং ইটারেট করার মৌলিক অপারেশনগুলির প্রদর্শন করে।

## গভীর ডুব
Elm-এর ডিকশনারিগুলি অভ্যন্তরীণভাবে একটি গঠন ব্যবহার করে যা একটি AVL গাছ নামে পরিচিত - এক ধরণের স্ব-সামঞ্জস্যপূর্ণ বাইনারি অনুসন্ধান গাছ। এই পছন্দটি insert, get, এবং remove এর মতো অপারেশনগুলির ভালো কার্যকারিতা (লগারিদমিক সময় জটিলতা) নিশ্চিত করে এবং ডেটা হ্যান্ডলিংয়ে সাধারণতার সাথে ব্যালেন্স রাখে।

Elm-এর `Dict`-এর শক্তি সত্ত্বেও, এটি একটি এক-আকারের-সব-ফিট সমাধান নয়। সংগ্রহস্থলগুলি যেগুলি অর্ডার করা অথবা ক্রমানুসারে ইটারেট করা প্রয়োজন, তার জন্য List বা Array আরো উপযুক্ত হতে পারে। তাছাড়া, যখন একটি নির্ধারিত সেট অফ পরিচিত কীগুলির সাথে কাজ করা হচ্ছে, কাস্টম টাইপস (Elm-এর সংস্করণের এনামগুলি) আপনার কোডের উদ্দেশ্য এবং টাইপ নিরাপত্তা উন্নতি করতে পারে।

Elm-এর ইকোসিস্টেমে, `Dict` একটি বিশ্বস্ত উপায় প্রদান করে যেখানে কী-মান জোড়াগুলির সংগ্রহ পরিচালনা করা হয় যেখানে কীগুলি অনন্য এবং ক্রমের বিষয় না। যদিও নতুন বা আরও উন্নত কাঠামো প্রকাশ পেতে পারে, `Dict` মডিউল এর সাধারণতা এবং এসোসিয়েটিভ অ্যারেগুলি হ্যান্ডলিংয়ে দক্ষতার জন্য Elm প্রোগ্রামারের টুলকিটে একটি মৌলিক সরঞ্জাম হিসেবে রয়ে যায়।
