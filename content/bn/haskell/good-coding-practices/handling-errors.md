---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:02.787115-06:00
description: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09DF\
  \u09C7 \u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09B8\u09BE\u09AE\u09BE\u09B2 \u09A6\
  \u09C7\u0993\u09DF\u09BE \u09AE\u09BE\u09A8\u09C7 \u0985\u09AA\u09CD\u09B0\u09A4\
  \u09CD\u09AF\u09BE\u09B6\u09BF\u09A4 \u09AA\u09B0\u09BF\u09B8\u09CD\u09A5\u09BF\u09A4\
  \u09BF\u2014\u09AF\u09BE \u09AD\u09C1\u09B2 \u09B9\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7 \u09A4\u09BE \u09B8\u09BE\u09AE\u09BE\u09B2 \u09A6\u09C7\u0993\u09DF\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09C7\u09A8 \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\
  \u09A4 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AF\u09C7 \u09A4\u09BE\
  \u09A6\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.091805-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09DF\
  \u09C7 \u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09B8\u09BE\u09AE\u09BE\u09B2 \u09A6\
  \u09C7\u0993\u09DF\u09BE \u09AE\u09BE\u09A8\u09C7 \u0985\u09AA\u09CD\u09B0\u09A4\
  \u09CD\u09AF\u09BE\u09B6\u09BF\u09A4 \u09AA\u09B0\u09BF\u09B8\u09CD\u09A5\u09BF\u09A4\
  \u09BF\u2014\u09AF\u09BE \u09AD\u09C1\u09B2 \u09B9\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7 \u09A4\u09BE \u09B8\u09BE\u09AE\u09BE\u09B2 \u09A6\u09C7\u0993\u09DF\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09C7\u09A8 \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\
  \u09A4 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AF\u09C7 \u09A4\u09BE\
  \u09A6\u09C7\u09B0\u2026"
title: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u0997\u09C1\u09B2\u09BF \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE"
weight: 16
---

## কি এবং কেন?
প্রোগ্রামিংয়ে ত্রুটি সামাল দেওয়া মানে অপ্রত্যাশিত পরিস্থিতি—যা ভুল হতে পারে তা সামাল দেওয়া। প্রোগ্রামাররা এটি করেন নিশ্চিত করার জন্য যে তাদের প্রোগ্রামগুলি এই পরিস্থিতিগুলি সহজে মোকাবেলা করতে পারে, ক্র্যাশ হওয়া বা ভুল ফলাফল উত্পন্ন করা ছাড়া।

## কিভাবে:
হাস্কেল `Maybe` এবং `Either` এর মতো টাইপের মাধ্যমে ত্রুটিগুলি দৃঢ়ভাবে সামাল দেয়। এখানে একটি দ্রুত দেখা রইল:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- শূন্য দ্বারা ভাগ করা যাবে না, তাই আমরা Nothing ফেরত দিচ্ছি।
safeDivide x y = Just (x `div` y)  -- অন্যান্য ক্ষেত্রে, আমরা সব ঠিক থাকলে, ফলাফল একটি Just এ ফেরত দিই।

-- এটি ব্যবহারে দেখা যাক:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

আরো জটিল ত্রুটি সামাল দেওয়ার জন্য, `Either` এর ব্যবহার ঘটে:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Divide by zero error."  -- এইবার, ত্রুটি একটি বার্তা সহ ফেরত আসে।
safeDivideEither x y = Right (x `div` y)

-- এবং ব্যবহারে:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Divide by zero error."
```

## গভীর ডুব
হাস্কেল বিশ্বে, ত্রুটি সামাল দেওয়ার একটি দীর্ঘ ইতিহাস আছে। পুরানো দিনে, ত্রুটিগুলি আপনার সম্পূর্ণ প্রোগ্রামকে বিপন্ন করে ফেলতে পারত—কোনো মজা নয়। হাস্কেলের টাইপ সিস্টেম এই সমস্যাকে অনেক কম সম্ভাব্য করে তোলে। আমাদের কাছে `Maybe` এবং `Either` আছে, কিন্তু `Exceptions` এবং `IO` এর মত অন্যান্য বিকল্প আছে বিভিন্ন পরিস্থিতির জন্য।

`Maybe` সরল: যদি সব ঠিক থাকে তাহলে আপনি `Just` কিছু পান, অথবা যদি না থাকে তাহলে `Nothing` পান। `Either` এটি আরো উন্নতি করে, আপনাকে একটি ত্রুটি বার্তা (`Left`) অথবা একটি সফল ফলাফল (`Right`) ফেরত দিতে দেয়।

উভয়ই পরিশুদ্ধ, অর্থাৎ তারা বাইরের দুনিয়ার সাথে ঘাটাঘাটি করে না – হাস্কেলে এটি একটি বড় ব্যাপার। আমরা অন্যান্য ভাষায় যে অযাচিত ব্যতিক্রমের সমস্যায় ভুগি তা থেকে বাঁচি।

যারা `Maybe` এবং `Either` দিয়ে সন্তুষ্ট নন, তাদের জন্য `Control.Exception` এর মতো লাইব্রেরিগুলি বেশি প্রথাগত, ইম্পেরেটিভ-স্টাইলের ত্রুটি সামাল দেওয়ার মাধ্যমে ব্যতিক্রম সরবরাহ করে। কিন্তু তাদের খুব বেশি ব্যবহার জিনিসগুলিকে জটিল করতে পারে, তাই সম্প্রদায় প্রায়ই টাইপগুলির দিকে ঝুঁকে থাকে।

## আরও দেখুন
গভীরে ডুব দিতে:

- হাস্কেলের নিজস্ব ডকুমেন্টেশন: [Haskell](https://haskell.org/documentation)
- শিক্ষানবিসদের জন্য দারুণ: ["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
