---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:35.292492-06:00
description: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\
  \u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0987\u09A8\
  \u09CD\u099F\u09BE\u09B0\u09A8\u09C7\u099F\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 \u098F\u09B0 \u09A1\u09C7\u099F\u09BE \u0986\u09B9\u09B0\u09A3 \u0995\
  \u09B0\u09BE; \u098F\u099F\u09BE \u09AF\u09C7\u09A8 \u09B8\u09CD\u09A5\u09BE\u09A8\
  \u09C0\u09AF\u09BC\u09AD\u09BE\u09AC\u09C7 \u09AA\u09A1\u09BC\u09BE \u09AC\u09BE\
  \ \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u0995\u09AA\u09BF \u09B8\
  \u0982\u09B0\u0995\u09CD\u09B7\u09A3 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.081329-06:00'
model: gpt-4-0125-preview
summary: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\u09A8\
  \u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0987\u09A8\u09CD\
  \u099F\u09BE\u09B0\u09A8\u09C7\u099F\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\
  \u09C7 \u098F\u09B0 \u09A1\u09C7\u099F\u09BE \u0986\u09B9\u09B0\u09A3 \u0995\u09B0\
  \u09BE; \u098F\u099F\u09BE \u09AF\u09C7\u09A8 \u09B8\u09CD\u09A5\u09BE\u09A8\u09C0\
  \u09AF\u09BC\u09AD\u09BE\u09AC\u09C7 \u09AA\u09A1\u09BC\u09BE \u09AC\u09BE \u09AA\
  \u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u0995\u09AA\u09BF \u09B8\u0982\
  \u09B0\u0995\u09CD\u09B7\u09A3 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\
  \u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09B8\u09CD\u0995\u09CD\u09B0\u09CD\u09AF\
  \u09BE\u09AA \u0995\u09B0\u09BE, \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09C7\u09AC\
  \u09BE\u0997\u09C1\u09B2\u09BF\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AE\u09BF\u09A5\
  \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0995\u09B0\u09BE, \u09AC\
  \u09BE \u09B8\u09BE\u0987\u099F\u0997\u09C1\u09B2\u09BF\u09B0 \u09AE\u09BF\u09B0\
  \u09B0 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8\u0964."
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

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
