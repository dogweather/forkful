---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:21.128665-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09A8\u09AE\u09C1\u09A8\u09BE\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F."
lastmod: '2024-04-05T21:53:52.451744-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
weight: 7
---

## কিভাবে:
```Haskell
-- `length` ফাংশন ব্যবহার করে
main = do
    let myString = "Hello, Haskell!"
    print $ length myString
```

নমুনা আউটপুট:
```
15
```

## গভীর ডুব
Haskell একটি নিখুঁত ফাংশনাল ভাষা যেখানে স্ট্রিংগুলি অক্ষরের তালিকা হিসেবে প্রতিনিধিত্ব করা হয়। `length` ফাংশনটি প্রিলিউডের (Haskell প্রোগ্রামে আমদানি করা ডিফল্ট লাইব্রেরি) অংশ, এই উপস্থাপনা অনুযায়ী কাজ করে।

ঐতিহাসিকভাবে, Haskell-এ স্ট্রিংগুলিকে তালিকা হিসেবে দেখা একটি স্বাভাবিক বাছাই ছিল কারণ এটি সাদাসিধা এবং লিস্প কর্তৃক একই সিদ্ধান্ত গ্রহণ (যা অনেক ফাংশনাল ভাষাকে প্রভাবিত করেছে)। `length` ফাংশনটি কেবল এই তালিকায় উপাদানগুলি গণনা করে।

তবে, `length` হল O(n), অর্থাৎ ফাংশনটি স্ট্রিং-এর দৈর্ঘ্যের সাথে আনুপাতিক সময় নেবে। সংক্ষিপ্ত স্ট্রিং-এর ক্ষেত্রে এটি সমস্যা নয়, তবে লম্বা স্ট্রিং-এর জন্য, এটি অকার্যকর হতে পারে।

বিকল্পগুলো অন্তর্ভুক্ত:
- `text` প্যাকেজ থেকে `Text`, Unicode টেক্স্টের জন্য আরও দক্ষ গঠন।
- বাইনারি বা ASCII ডেটার জন্য `bytestring` প্যাকেজ থেকে `ByteString` ব্যবহার করা।

উভয়ই তাদের যথাযথ ডেটা গঠনের জন্য অপ্টিমাইজড `length` ফাংশন প্রদান করে।

বাস্তবায়নের দিক থেকে, `length` ফাংশনের একটি মৌলিক সংস্করণ এরকম দেখতে পারে:

```Haskell
myLength :: [a] -> Int
myLength [] = 0          -- একটি খালি তালিকার দৈর্ঘ্য হল 0
myLength (_:xs) = 1 + myLength xs  -- বাকি তালিকার জন্য পুনরাবৃত্তিমূলকভাবে 1 যোগ করা
```

`Text` এবং `ByteString` ডেটা টাইপের জন্য, তাদের নিজস্ব অভ্যন্তরীণ বাস্তবায়নের বিবরণ রয়েছে যা তাদেরকে সাধারণ অক্ষরের একটি সরল লিংক্ড লিস্টের চেয়ে দক্ষ করে তোলে।

## দেখুন এবং শিখুন
- [Haskell `length` অফিসিয়াল ডকুমেন্টেশন](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:length)
- [`text` প্যাকেজ Hackage-এ](https://hackage.haskell.org/package/text)
- [`bytestring` প্যাকেজ Hackage-এ](https://hackage.haskell.org/package/bytestring)
- [Learn You a Haskell for Great Good! (একটি প্রাথমিক বই)](http://learnyouahaskell.com/chapters)
