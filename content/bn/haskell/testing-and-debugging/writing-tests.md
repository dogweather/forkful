---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:45.961375-06:00
description: "Haskell \u098F \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982 \u09AE\u09BE\
  \u09A8\u09C7 \u09B9\u099A\u09CD\u099B\u09C7 \u09B8\u09CD\u09AC\u09DF\u0982\u0995\
  \u09CD\u09B0\u09BF\u09DF \u099A\u09C7\u0995\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\
  \u09AF\u09AE\u09C7 \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09BE\
  \ \u09AF\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\
  \u09C1\u09B2\u09BF \u09AA\u09CD\u09B0\u09A4\u09CD\u09AF\u09BE\u09B6\u09BF\u09A4\u09AD\
  \u09BE\u09AC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u099B\u09C7\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09C7\u09A8 \u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u0997\u09C1\u09B2\
  \u09BF \u09A4\u09BE\u09DC\u09BE\u09A4\u09BE\u09DC\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.086999-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u098F \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982 \u09AE\u09BE\u09A8\
  \u09C7 \u09B9\u099A\u09CD\u099B\u09C7 \u09B8\u09CD\u09AC\u09DF\u0982\u0995\u09CD\
  \u09B0\u09BF\u09DF \u099A\u09C7\u0995\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09BE \u09AF\
  \u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\
  \u09B2\u09BF \u09AA\u09CD\u09B0\u09A4\u09CD\u09AF\u09BE\u09B6\u09BF\u09A4\u09AD\u09BE\
  \u09AC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u099B\u09C7\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09C7\u09A8 \u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u0997\u09C1\u09B2\
  \u09BF \u09A4\u09BE\u09DC\u09BE\u09A4\u09BE\u09DC\u09BF\u2026"
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
---

{{< edit_this_page >}}

## কী এবং কেন?

Haskell এ টেস্টিং মানে হচ্ছে স্বয়ংক্রিয় চেকের মাধ্যমে নিশ্চিত করা যে আপনার ফাংশনগুলি প্রত্যাশিতভাবে কাজ করছে। প্রোগ্রামাররা এটি করেন ত্রুটিগুলি তাড়াতাড়ি ধরার জন্য, রিফ্যাক্টরিং সহজ করতে, এবং আচরণ নথিভুক্ত করে কোডবেজকে আরো রক্ষণশীল এবং স্কেলেবল করে তোলা।

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
