---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:16:49.544893-06:00
description: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\
  \u09A7\u09BE\u09A8 \u0993 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\u09BE\
  \u09AA\u09A8 \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982\u0997\u09C1\u09B2\u09BF \u0996\u09C1\u0981\u099C\u09C7 \u09AC\u09C7\
  \u09B0 \u0995\u09B0\u09A4\u09C7 \u098F\u09AC\u0982 \u09A4\u09BE\u09A6\u09C7\u09B0\
  \ \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09A4\u09C7 \u09A6\
  \u09C7\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u0995\u09CB\u09A1 \u0986\u09AA\u09A1\u09C7\u099F \u0995\u09B0\
  \u09A4\u09C7, \u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0 \u0995\
  \u09B0\u09A4\u09C7, \u09AC\u09BE \u09A6\u09CD\u09B0\u09C1\u09A4\u09A4\u09BE\u09B0\
  \u2026"
lastmod: '2024-03-17T18:47:44.066280-06:00'
model: gpt-4-0125-preview
summary: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\
  \u09A7\u09BE\u09A8 \u0993 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\u09BE\
  \u09AA\u09A8 \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982\u0997\u09C1\u09B2\u09BF \u0996\u09C1\u0981\u099C\u09C7 \u09AC\u09C7\
  \u09B0 \u0995\u09B0\u09A4\u09C7 \u098F\u09AC\u0982 \u09A4\u09BE\u09A6\u09C7\u09B0\
  \ \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09A4\u09C7 \u09A6\
  \u09C7\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u0995\u09CB\u09A1 \u0986\u09AA\u09A1\u09C7\u099F \u0995\u09B0\
  \u09A4\u09C7, \u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0 \u0995\
  \u09B0\u09A4\u09C7, \u09AC\u09BE \u09A6\u09CD\u09B0\u09C1\u09A4\u09A4\u09BE\u09B0\
  \u2026"
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
weight: 10
---

## কি এবং কেন?

টেক্সট অনুসন্ধান ও প্রতিস্থাপন আপনাকে স্ট্রিংগুলি খুঁজে বের করতে এবং তাদের পরিবর্তন করতে দেয়। প্রোগ্রামাররা কোড আপডেট করতে, রিফ্যাক্টর করতে, বা দ্রুততার সাথে ডেটা পরিবর্তন করতে এই প্রক্রিয়া ব্যবহার করে।

## কিভাবে:

চলুন আমরা Haskell ব্যবহার করে টেক্সট অনুসন্ধান এবং প্রতিস্থাপন করি। আমরা ইউনিকোড টেক্সট হ্যান্ডলিং এবং দক্ষতার জন্য `Data.Text` ব্যবহার করব। `Data.Text` এভাবে ইম্পোর্ট করতে নিশ্চিত করুন:

```haskell
import qualified Data.Text as T
```

এখন, আসুন আমরা একটি টেক্সটের "hello" সকল উদাহরণকে "hi" দিয়ে প্রতিস্থাপন করি:

```haskell
replaceText :: T.Text -> T.Text -> T.Text -> T.Text
replaceText old new = T.replace old new

main :: IO ()
main = do
  let originalText = T.pack "hello world, hello Haskell!"
  let newText = replaceText (T.pack "hello") (T.pack "hi") originalText
  print newText -- "hi world, hi Haskell!"
```

`replace` ফাংশনটি প্রধান কাজ করে। আমরা স্পষ্টতার জন্য এটিকে `replaceText` এ মোড়ানো আছে।

## গভীর ডুব

Haskell-এর টেক্সট প্রতিস্থাপন ফাংশনগুলি যেমন `T.replace` হেসেবের উপরে নির্মিত, Haskell-এর অ্যারে প্রক্রিয়া ক্ষমতাগুলির উপর। পেছনে তাকালে, Haskell প্রথম ৮০ দশকে ভাবনা করা হয়েছিল, ফাংশনাল প্রোগ্রামিং-এ মনোনিবেশের সাথে। এই প্রতিমান ইমোটেবিলিটি এবং শক্তিশালী টাইপ সিস্টেমের কারণে টেক্সট প্রতিস্থাপনের মতো অপারেশন সুরুচিপূর্ণ এবং কম ত্রুটিপূর্ণ করে তোলে।

বিকল্প হিসেবে, আপনি ম্যানুয়ালি টেক্সট ইটারেট করতে এবং সাবস্ট্রিংগুলি প্রতিস্থাপন করতে পারেন, কিন্তু তা বেশি ত্রুটিপূর্ণ এবং অকার্যকর।

`Data.Text` লাইব্রেরিটি `String` টাইপের চেয়ে একটি ভিন্ন অভ্যন্তরীণ উপস্থাপন ব্যবহার করে (যা কেবলমাত্র অক্ষরের একটি তালিকা), তাই এটি বৃহত্তর পাঠ্য অপারেশনের জন্য আরও ভাল উপযুক্ত। `T.replace` ফাংশনটি স্ট্রিং অনুসন্ধানের জন্য দক্ষ অ্যালগরিদম ব্যবহার করে, যা বড় পাঠ্যের জন্য ভাল কর্মক্ষমতা প্রদান করে।

## আরও দেখুন

`Data.Text` এর জন্য আরও জানতে:

- [Hackage-এ Text package](https://hackage.haskell.org/package/text)

Haskell-এর স্ট্রিং ম্যানিপুলেশনের উপর আরও পড়াশোনা বিবেচনা করুন:

- [Haskell Wiki স্ট্রিংস অন](https://wiki.haskell.org/Strings)
- [Learn You a Haskell for Great Good! on Text](http://learnyouahaskell.com/input-and-output#files-and-streams)
