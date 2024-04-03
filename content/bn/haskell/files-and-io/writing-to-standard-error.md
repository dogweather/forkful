---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:41.543859-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Haskell-\u098F, `System.IO` \u09AE\
  \u09A1\u09BF\u0989\u09B2\u09C7\u09B0 \u09B8\u09BE\u09B9\u09BE\u09AF\u09CD\u09AF\u09C7\
  \ stderr \u09B2\u09C7\u0996\u09BE \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09B8\u09AE\
  \u09CD\u09AD\u09AC\u0964 \u09A8\u09BF\u099A\u09C7 \u098F\u0995\u099F\u09BF \u09AC\
  \u09C7\u09B8\u09BF\u0995 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0996\
  \u09BE\u09A8\u09CB \u09B9\u09B2\u09CB."
lastmod: '2024-03-17T18:47:44.101738-06:00'
model: gpt-4-0125-preview
summary: "Haskell-\u098F, `System.IO` \u09AE\u09A1\u09BF\u0989\u09B2\u09C7\u09B0 \u09B8\
  \u09BE\u09B9\u09BE\u09AF\u09CD\u09AF\u09C7 stderr \u09B2\u09C7\u0996\u09BE \u09B8\
  \u09B0\u09BE\u09B8\u09B0\u09BF \u09B8\u09AE\u09CD\u09AD\u09AC\u0964 \u09A8\u09BF\
  \u099A\u09C7 \u098F\u0995\u099F\u09BF \u09AC\u09C7\u09B8\u09BF\u0995 \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2\u09CB\
  ."
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কিভাবে:
Haskell-এ, `System.IO` মডিউলের সাহায্যে stderr লেখা সরাসরি সম্ভব। নিচে একটি বেসিক উদাহরণ দেখানো হলো:

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "এটি একটি ত্রুটি বার্তা।"
```

এই প্রোগ্রামের stderr এ আউটপুট হবে:

```
এটি একটি ত্রুটি বার্তা।
```

যদি আপনি একটি আরও জটিল অ্যাপ্লিকেশনে কাজ করেন, অথবা লগিং (ত্রুটি সহ) এর উপরে ভাল নিয়ন্ত্রণ পেতে চান, আপনি তৃতীয় পক্ষের লাইব্রেরি চয়ন করতে পারেন। একটি জনপ্রিয় পছন্দ হল `monad-logger` যা Haskell প্রোগ্রামিংয়ের `mtl` স্টাইলের সাথে একীভূত হয়। এখানে `monad-logger` ব্যবহার করে একটি ছোট স্নিপেট দেওয়া হলো:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "এটি একটি ত্রুটি বার্তা যা monad-logger ব্যবহার করে।"
```

চালানো হলে, `monad-logger` ভার্সনও একটি ত্রুটি বার্তা আউটপুট করে, কিন্তু এটি কনফিগারেশনের উপর নির্ভরশীল সময়তালিকা বা লগ লেভেলের মত আরও প্রসঙ্গ সহ সজ্জিত হয়ে আসে:

```
[Error] এটি একটি ত্রুটি বার্তা যা monad-logger ব্যবহার করে।
```

উভয় পদ্ধতিই stderr লিখনের উদ্দেশ্যে কাজ করে, আপনার অ্যাপ্লিকেশনের জটিলতা এবং প্রয়োজনের উপর পছন্দের সিদ্ধান্ত অনেকাংশে নির্ভর করে।
