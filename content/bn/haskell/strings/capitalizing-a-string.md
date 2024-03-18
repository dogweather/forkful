---
title:                "স্ট্রিং এর প্রথম অক্ষর বড় হাতের করা"
date:                  2024-03-17T17:45:42.064331-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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
