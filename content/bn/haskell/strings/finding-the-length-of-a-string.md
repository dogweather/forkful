---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:21.128665-06:00
description: "Haskell-\u098F \u0995\u09CB\u09A8\u09CB \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982-\u098F\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u09A8\u09BF\
  \u09B0\u09CD\u09A3\u09AF\u09BC \u0995\u09B0\u09BE\u09B0 \u09AE\u09BE\u09A8\u09C7\
  \ \u09B9\u09B2 \u098F\u09A4\u09C7 \u0995\u09A4\u0997\u09C1\u09B2\u09BF \u0985\u0995\
  \u09CD\u09B7\u09B0 \u0986\u099B\u09C7 \u09A4\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\
  \u09BF\u09B7\u09CD\u099F \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3 \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\
  \u0987 \u09B2\u09C1\u09AA \u0987\u099F\u09BE\u09B0\u09C7\u099F\u09B0\u09B8 \u09A8\
  \u09BF\u09AF\u09BC\u09A8\u09CD\u09A4\u09CD\u09B0\u09A3, \u0987\u09A8\u09AA\u09C1\
  \u099F \u09AF\u09BE\u099A\u09BE\u0987,\u2026"
lastmod: '2024-03-17T18:47:44.072483-06:00'
model: gpt-4-0125-preview
summary: "Haskell-\u098F \u0995\u09CB\u09A8\u09CB \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982-\u098F\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u09A8\u09BF\u09B0\
  \u09CD\u09A3\u09AF\u09BC \u0995\u09B0\u09BE\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\
  \u09B2 \u098F\u09A4\u09C7 \u0995\u09A4\u0997\u09C1\u09B2\u09BF \u0985\u0995\u09CD\
  \u09B7\u09B0 \u0986\u099B\u09C7 \u09A4\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\
  \u09B7\u09CD\u099F \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3 \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987\
  \ \u09B2\u09C1\u09AA \u0987\u099F\u09BE\u09B0\u09C7\u099F\u09B0\u09B8 \u09A8\u09BF\
  \u09AF\u09BC\u09A8\u09CD\u09A4\u09CD\u09B0\u09A3, \u0987\u09A8\u09AA\u09C1\u099F\
  \ \u09AF\u09BE\u099A\u09BE\u0987,\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
weight: 7
---

## কি এবং কেন?

Haskell-এ কোনো স্ট্রিং-এর দৈর্ঘ্য নির্ণয় করার মানে হল এতে কতগুলি অক্ষর আছে তা নির্দিষ্ট করা। প্রোগ্রামারগণ প্রায়ই লুপ ইটারেটরস নিয়ন্ত্রণ, ইনপুট যাচাই, সাইজ বরাদ্দ অথবা ডিবাগিং উদ্দেশ্যে এই তথ্য প্রয়োজন পান।

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
