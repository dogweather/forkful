---
title:                "একটি ওয়েবপেজ ডাউনলোড করা"
date:                  2024-03-17T17:47:35.292492-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

ওয়েব পেজ ডাউনলোড করা মানে ইন্টারনেটের মাধ্যমে এর ডেটা আহরণ করা; এটা যেন স্থানীয়ভাবে পড়া বা প্রক্রিয়া করার জন্য একটি কপি সংরক্ষণ করা। প্রোগ্রামাররা এটি কন্টেন্ট স্ক্র্যাপ করা, ওয়েব সেবাগুলির সাথে মিথস্ক্রিয়া করা, বা সাইটগুলির মিরর তৈরি করার জন্য করে থাকেন।

## কিভাবে:

আসুন দেখি Haskell-এর `http-conduit` লাইব্রেরি ব্যবহার করে একটি সহজ উদাহরণের মাধ্যমে। প্রথমে, `cabal install http-conduit` ব্যবহার করে এটি ইন্সটল করুন। তারপরে:

```Haskell
import Network.HTTP.Conduit -- প্রধান নেটওয়ার্ক লাইব্রেরি
import qualified Data.ByteString.Lazy as L -- আমাদের Lazy ByteStrings প্রয়োজন হবে

-- ওয়েব পেজ ডাউনলোড করার ফাংশন
downloadPage :: String -> IO L.ByteString
downloadPage url = simpleHttp url

main :: IO ()
main = do
    -- ফাংশনটি ব্যবহার করে একটি পেজ ডাউনলোড করুন
    content <- downloadPage "http://example.com"
    -- কন্টেন্ট দিয়ে কিছু করুন, যেমন এটি প্রিন্ট করা
    L.putStr content
```

এটি চালালে, আপনি `http://example.com`-এর HTML আপনার স্ক্রীনে দেখতে পাবেন।

## গভীর ডুব

Haskell-এ HTTP অনুরোধগুলি সর্বদা এত পরিপাটি ছিল না। `HTTP` মতো পুরানো লাইব্রেরিগুলি বেশি boilerplate কোড প্রয়োজন ছিল। `http-conduit` এর সাথে, জটিলতা অপসারিত হয়েছে।

অন্যান্য পদ্ধতি আছে, যেমন একটি শেল স্ক্রিপ্টে `wget` কমান্ড বা Python-এর `requests` লাইব্রেরি। কিন্তু এগুলি Haskell-এর ফাংশনাল পরিবেশে সর্বদা এত দক্ষ বা প্রকাশপূর্ণ নাও হতে পারে।

অন্তরালে, `http-conduit` একটি ম্যানেজারকে ব্যবহার করে কানেকশন পুলিং এবং HTTP1.1 এর জন্য Keep-Alive পরিচালনা করে, যা এটিকে একাধিক অনুরোধের জন্য আরও দক্ষ করে তোলে।

## আরো দেখুন

- `http-conduit`-এর আরও উন্নত ব্যবহারের জন্য: [http-conduit on Hackage](https://hackage.haskell.org/package/http-conduit)
- ByteString বুঝতে: [ByteString on Hackage](https://hackage.haskell.org/package/bytestring)
