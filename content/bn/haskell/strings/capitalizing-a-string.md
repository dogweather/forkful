---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:42.064331-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u09AC\u09DC \u09B9\u09BE\u09A4\u09C7\u09B0 \u0995\u09B0\u09C7 \u09B2\u09C7\
  \u0996\u09BE \u0985\u09B0\u09CD\u09A5\u09BE\u09A4\u09CD\u200D \u0995\u09CD\u09AF\
  \u09BE\u09AA\u09BF\u099F\u09BE\u09B2\u09BE\u0987\u099C \u0995\u09B0\u09BE \u09B9\
  \u09B2\u09CB \u09A6\u09C7\u0993\u09DF\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u099F\u09BF\u09B0 \u09AA\u09CD\u09B0\u09A5\u09AE \u0985\u0995\u09CD\u09B7\
  \u09B0\u0995\u09C7 \u09AC\u09DC \u0985\u0995\u09CD\u09B7\u09B0\u09C7 (\u0986\u09AA\
  \u09BE\u09B0\u0995\u09C7\u09B8) \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0\
  \ \u0995\u09B0\u09BE, \u098F\u09AC\u0982 \u09AC\u09BE\u0995\u09BF \u0985\u0995\u09CD\
  \u09B7\u09B0\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u099B\u09CB\u099F\u2026"
lastmod: '2024-03-17T18:47:44.064121-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u09AC\u09DC \u09B9\u09BE\u09A4\u09C7\u09B0 \u0995\u09B0\u09C7 \u09B2\u09C7\
  \u0996\u09BE \u0985\u09B0\u09CD\u09A5\u09BE\u09A4\u09CD\u200D \u0995\u09CD\u09AF\
  \u09BE\u09AA\u09BF\u099F\u09BE\u09B2\u09BE\u0987\u099C \u0995\u09B0\u09BE \u09B9\
  \u09B2\u09CB \u09A6\u09C7\u0993\u09DF\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u099F\u09BF\u09B0 \u09AA\u09CD\u09B0\u09A5\u09AE \u0985\u0995\u09CD\u09B7\
  \u09B0\u0995\u09C7 \u09AC\u09DC \u0985\u0995\u09CD\u09B7\u09B0\u09C7 (\u0986\u09AA\
  \u09BE\u09B0\u0995\u09C7\u09B8) \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0\
  \ \u0995\u09B0\u09BE, \u098F\u09AC\u0982 \u09AC\u09BE\u0995\u09BF \u0985\u0995\u09CD\
  \u09B7\u09B0\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u099B\u09CB\u099F\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
weight: 2
---

## কি এবং কেন?
একটি স্ট্রিংকে বড় হাতের করে লেখা অর্থাত্‍ ক্যাপিটালাইজ করা হলো দেওয়া স্ট্রিংটির প্রথম অক্ষরকে বড় অক্ষরে (আপারকেস) রূপান্তর করা, এবং বাকি অক্ষরগুলিকে ছোট অক্ষরে (লোয়ারকেস) রাখা। প্রোগ্রামাররা আউটপুট ফরম্যাটিং, টেক্সটে গ্রামাটিক্যাল সঠিকতা মেনে চলা, অথবা জেনারেটেড ডেটার পাঠযোগ্যতা উন্নতির জন্য এটি করে থাকেন।

## কিভাবে:
Haskell এ, আপনি স্ট্যান্ডার্ড লাইব্রেরি ব্যবহার করে কোন তৃতীয় পক্ষের লাইব্রেরির প্রয়োজন ছাড়াই একটি স্ট্রিং ক্যাপিটালাইজ করতে পারেন।

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- নমুনা ব্যবহার:
main = putStrLn $ capitalize "hello world"
```

আউটপুট:
```
Hello world
```

আরো জটিল সিনারিওর জন্য অথবা সুবিধার জন্য আপনি `text` নামের তৃতীয় পক্ষের লাইব্রেরি ব্যবহার করতে চাইতে পারেন, যা Haskell এ কার্যকরী স্ট্রিং ম্যানিপুলেশনের জন্য জনপ্রিয়।

প্রথমে, আপনার প্রজেক্টের নির্ভরশীলতাগুলিতে `text` যুক্ত করতে হবে। তারপর, আপনি এর ফাংশনগুলি ব্যবহার করে নিম্নলিখিতভাবে একটি স্ট্রিং ক্যাপিটালাইজ করতে পারেন:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- টেক্সট লাইব্রেরী সহ নমুনা ব্যবহার:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

আউটপুট:
```
Hello world
```

এই দুটি উদাহরণ হাসকেল ব্যবহার করে স্ট্রিং ক্যাপিটালাইজ করার সহজ কিন্তু কার্যকরী উপায় দেখাচ্ছে, তৃতীয় পক্ষের লাইব্রেরিকে নিয়ে অথবা না নিয়ে।
