---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:07:31.174002-06:00
description: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u09A5\u09C7\u0995\
  \u09C7 \u09A1\u09BE\u099F\u09BE \u09AC\u09BE\u09B0 \u0995\u09B0\u09C7 \u0986\u09A8\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09C7\
  \ \u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\u09CD\
  \u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE \u09AC\u09B2\u09C7\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\
  \u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7 \u09AD\u09C7\u09B0\u09BF\u09AF\
  \u09BC\u09C7\u09AC\u09B2\u0997\u09C1\u09B2\u09BF \u099F\u09CD\u09B0\u09CD\u09AF\u09BE\
  \u0995 \u0995\u09B0\u09A4\u09C7, \u09AB\u09CD\u09B2\u09CB \u09AC\u09C1\u099D\u09A4\
  \u09C7 \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.086005-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u09A5\u09C7\u0995\
  \u09C7 \u09A1\u09BE\u099F\u09BE \u09AC\u09BE\u09B0 \u0995\u09B0\u09C7 \u0986\u09A8\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09C7\
  \ \u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\u09CD\
  \u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE \u09AC\u09B2\u09C7\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\
  \u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7 \u09AD\u09C7\u09B0\u09BF\u09AF\
  \u09BC\u09C7\u09AC\u09B2\u0997\u09C1\u09B2\u09BF \u099F\u09CD\u09B0\u09CD\u09AF\u09BE\
  \u0995 \u0995\u09B0\u09A4\u09C7, \u09AB\u09CD\u09B2\u09CB \u09AC\u09C1\u099D\u09A4\
  \u09C7 \u098F\u09AC\u0982 \u09AC\u09BF\u09B0\u0995\u09CD\u09A4\u09BF\u0995\u09B0\
  \ \u09AC\u09BE\u0997\u0997\u09C1\u09B2\u09BF \u09A0\u09BF\u0995 \u0995\u09B0\u09A4\
  \u09C7\u0964."
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কিভাবে:
Haskell এ ডিবাগ তথ্য প্রিন্ট করার একটি সোজাসাপ্টা উপায় হল `print` ফাংশন ব্যবহার করা, যা একটি মান নেয় যা `Show` টাইপক্লাসের একটি ইন্সট্যান্স এবং এটি কনসোলে আউটপুট করে।

```Haskell
main :: IO ()
main = do
  let number = 42
  print number
  putStrLn "Haskell এ ডিবাগিং খুবই সহজ!"

-- আউটপুট:
-- 42
-- Haskell এ ডিবাগিং খুবই সহজ!
```

আরো জটিল ডাটা স্ট্রাকচারের জন্য, সুন্দর প্রিন্টিং সক্রিয় করতে `Show` অনুমোদন করতে হবে:

```Haskell
data Cake = Chocolate | Vanilla deriving Show

debugFlavor :: Cake -> IO ()
debugFlavor flavor = print flavor

main :: IO ()
main = debugFlavor Chocolate

-- আউটপুট:
-- Chocolate
```

মাঝে মাঝে আমরা অস্থায়ী ডিবাগিং চাই যা পরে সহজে সরানো যায়। `Debug.Trace` মডিউল এখানে প্রবেশ করে।

```Haskell
import Debug.Trace (trace)

main :: IO ()
main = putStrLn $ trace "প্রথমে এটি প্রিন্ট হবে" "দ্বিতীয়বার এটি প্রিন্ট হবে"

-- আউটপুট:
-- প্রথমে এটি প্রিন্ট হবে
-- দ্বিতীয়বার এটি প্রিন্ট হবে
```

`trace` ফাংশনটি স্ট্রিংটি প্রিন্ট করে যখন মানটি মূল্যায়িত হয়, কিন্তু কোডের পরিষ্কার অংশে এটি একটি পার্শ্ব প্রভাব। এটি উপযোগী, কিন্তু সাবধানে ব্যবহার করুন!

## গভীর ডাইভ
পুরনো দিনগুলিতে, ডিবাগিং হয়তো ছিল "প্রিন্ট স্টেটমেন্ট" ট্রিক। Haskell এই সঙ্গে একটি ফাংশনাল মোড় এবং আরও পরিষ্কার ডিবাগ প্র্যাকটিসের জন্য টুলস অফার করে। পূর্বে আলোচিত `print` এবং `Debug.Trace` মডিউল অন্তর্ভুক্ত।

`print`-এর পরিবর্তনগুলিতে `putStrLn` স্ট্রিংগুলির জন্য এবং স্বয়ংক্রিয় নিউলাইন না হওয়ার ক্ষেত্রে `putStr` অন্তর্ভুক্ত থাকে। `Debug.Trace` এর ভেরিয়েন্ট যেমন `traceShow` সরাসরি `Show` ইনস্ট্যান্সগুলির সাথে কাজ করে, যা `show` কলটি বাঁচায়।

বাস্তবায়নের বিস্তারিত বিবরণ হল, `print` মূলত `putStrLn . show`। এটি stdout-এ যে১কোনো `Show` যোগ্য ডাটা প্রিন্ট করে। অন্যদিকে, `Debug.Trace` ফাংশনগুলি উন্নয়নের সময় অস্থায়ী ব্যবহারের জন্য মানা হয়। এগুলি পরিষ্কার কোডে প্রবেশ করে এবং নির্দেশক স্বচ্ছতার লঙ্ঘন করে, যা দীর্ঘায়িত ব্যবহারের ক্ষেত্রে নিষিদ্ধ।

গুরুত্বপূর্ণ অ্যাপ্লিকেশনগুলির জন্য লগিং লাইব্রেরিগুলি না ভুলে যেগুলো বেশি নিয়ন্ত্রণ এবং "ডিবাগ বাই প্রিন্ট" এর তুলনায় কম অফার করে।

## আরো দেখুন
- `Debug.Trace` ডকুমেন্টেশন: [https://hackage.haskell.org/package/base/docs/Debug-Trace.html](https://hackage.haskell.org/package/base/docs/Debug-Trace.html)
- ডিবাগিং সম্পর্কে Haskell Wiki: [https://wiki.haskell.org/Debugging](https://wiki.haskell.org/Debugging)
- কেন `Debug.Trace` ব্যবহার করা খারাপ প্র্যাকটিস এবং এর পরিবর্তে কি করা উচিৎ, এই বিষয়ে একটি ভালো আলোচনা: [https://stackoverflow.com/questions/7741400/why-is-using-debug-trace-considered-bad-practice](https://stackoverflow.com/questions/7741400/why-is-using-debug-trace-considered-bad-practice)
