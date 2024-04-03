---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:45.961375-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Haskell \u09AC\u09BF\u09AD\u09BF\
  \u09A8\u09CD\u09A8 \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982 \u09AB\u09CD\u09B0\
  \u09C7\u09AE\u0993\u09DF\u09BE\u09B0\u09CD\u0995\u09C7\u09B0 \u09B8\u09BE\u09AA\u09CB\
  \u09B0\u09CD\u099F \u0995\u09B0\u09C7, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u09A6\
  \u09C1\u099F\u09BF \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09DF \u09B9\u09B2 `Hspec`\
  \ \u098F\u09AC\u0982 `QuickCheck`\u0964 Hspec \u0986\u09AA\u09A8\u09BE\u0995\u09C7\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09CD\u09AF\u09AF\u09CB\u0997\
  \u09CD\u09AF\u2026"
lastmod: '2024-03-17T18:47:44.086999-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u099F\u09C7\u09B8\u09CD\
  \u099F\u09BF\u0982 \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09DF\u09BE\u09B0\u09CD\u0995\
  \u09C7\u09B0 \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u0995\u09B0\u09C7, \u0995\
  \u09BF\u09A8\u09CD\u09A4\u09C1 \u09A6\u09C1\u099F\u09BF \u099C\u09A8\u09AA\u09CD\
  \u09B0\u09BF\u09DF \u09B9\u09B2 `Hspec` \u098F\u09AC\u0982 `QuickCheck`\u0964 Hspec\
  \ \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\
  \u09A1\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\
  \u09A0\u09CD\u09AF\u09AF\u09CB\u0997\u09CD\u09AF \u09AC\u09BF\u09AC\u09B0\u09A3\
  \ \u09B8\u0982\u099C\u09CD\u099E\u09BE\u09DF\u09BF\u09A4 \u0995\u09B0\u09A4\u09C7\
  \ \u0985\u09A8\u09C1\u09AE\u09A4\u09BF \u09A6\u09C7\u09DF \u09AF\u09C7\u0996\u09BE\
  \u09A8\u09C7 QuickCheck \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1 \u09AF\
  \u09BE \u09AA\u09C2\u09B0\u09A3 \u0995\u09B0\u09BE \u0989\u099A\u09BF\u09A4 \u09B8\
  \u09C7\u0987 \u0997\u09C1\u09A3\u09BE\u09AC\u09B2\u09C0 \u09AC\u09B0\u09CD\u09A3\
  \u09A8\u09BE \u0995\u09B0\u09C7 \u09B8\u09CD\u09AC\u09DF\u0982\u0995\u09CD\u09B0\
  \u09BF\u09DF\u09AD\u09BE\u09AC\u09C7 \u099F\u09C7\u09B8\u09CD\u099F\u0997\u09C1\u09B2\
  \u09BF \u099C\u09C7\u09A8\u09BE\u09B0\u09C7\u099F \u0995\u09B0\u09C7\u0964\n\n#."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কিভাবে:
Haskell বিভিন্ন টেস্টিং ফ্রেমওয়ার্কের সাপোর্ট করে, কিন্তু দুটি জনপ্রিয় হল `Hspec` এবং `QuickCheck`। Hspec আপনাকে আপনার কোডের জন্য মানব-পাঠ্যযোগ্য বিবরণ সংজ্ঞায়িত করতে অনুমতি দেয় যেখানে QuickCheck আপনার কোড যা পূরণ করা উচিত সেই গুণাবলী বর্ণনা করে স্বয়ংক্রিয়ভাবে টেস্টগুলি জেনারেট করে।

### Hspec ব্যবহার করে
প্রথমে, আপনার বিল্ড টুল কনফিগারেশনে (যেমন `stack.yaml` অথবা `cabal` ফাইল) `hspec` যোগ করুন। তারপর, `Test.Hspec` ইম্পোর্ট করুন এবং স্পেসিফিকেশন হিসেবে টেস্টগুলি লিখুন:

```haskell
-- ফাইল: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "দুটি সংখ্যা যোগ করে" $
    add 1 2 `shouldBe` 3

  it "শূন্য যোগ করলে প্রথম সংখ্যাটি ফেরত দেয়" $
    add 5 0 `shouldBe` 5
```

তারপর, আপনার বিল্ড টুল ব্যবহার করে টেস্টগুলি চালান, ফলাফল দেখতে পাবেন:

```
MyLib.add
  - দুটি সংখ্যা যোগ করে
  - শূন্য যোগ করলে প্রথম সংখ্যাটি ফেরত দেয়

০.০০০১ সেকেন্ডে সমাপ্ত
২ উদাহরণ, ০ ব্যর্থতা
```

### QuickCheck ব্যবহার করে
QuickCheck এর সাথে, আপনি এমন গুণাবলী প্রেস করেন যা আপনার ফাংশনগুলি পূরণ করা উচিত। আপনার প্রজেক্ট কনফিগারেশনে `QuickCheck` যোগ করুন, তারপর এটি ইম্পোর্ট করুন:

```haskell
-- ফাইল: test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative
```

এই টেস্টগুলি চালানোর সময় নির্দিষ্ট গুণাবলী চেক করার জন্য স্বয়ংক্রিয়ভাবে ইনপুটগুলি জেনারেট করবে:

```
+++ OK, ১০০ টেস্ট পাস করেছে।
+++ OK, ১০০ টেস্ট পাস করেছে।
```

Hspec এবং QuickCheck উদাহরণ উভয়ের ক্ষেত্রেই, টেস্ট সুইটগুলি নিজেদের কোডের সঠিকতা স্বয়ংক্রিয়ভাবে যাচাই করতে পারে এমন এক নির্বাহযোগ্য নথি হিসাবে কাজ করে।
