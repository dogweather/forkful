---
title:                "টেক্সট অনুসন্ধান এবং প্রতিস্থাপন"
date:                  2024-03-17T18:16:49.544893-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
