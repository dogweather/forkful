---
title:                "HTTP অনুরোধ প্রেরণ করা"
date:                  2024-03-17T18:17:34.377626-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
HTTP অনুরোধ পাঠানো হল ওয়েব সার্ভারে ডেটা বা ক্রিয়া চাওয়ার প্রক্রিয়া। প্রোগ্রামাররা API-এর সাথে মিথস্ক্রিয়া করতে, ওয়েব কন্টেন্ট সংগ্রহ করতে, বা সেবা গুলোর মধ্যে যোগাযোগ করতে এটি করে থাকে।

## কিভাবে:
চলুন মজার বিষয়ে চলে যাই। আপনার `http-client` এবং `http-client-tls` প্যাকেজ প্রয়োজন হবে। আপনার স্ট্যাক সেট করুন এবং এগুলোকে আপনার `package.yaml` অথবা `.cabal` ফাইলে যোগ করুন। তারপর, `stack build` বা উপযুক্ত কমান্ড চালান তাদের অনুসন্ধানের জন্য।

এখানে একটি সিম্পল GET অনুরোধ রয়েছে:

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "http://httpbin.org/get"
    response <- httpLbs request manager
    L8.putStrLn $ responseBody response
```

এটি `httpbin.org` থেকে পাওয়া JSON প্রিন্ট করবে।

## গভীরে দেখা
পূর্বে, Haskell এর HTTP অনুরোধ কম সোজা ছিল, কিন্তু `http-client` এর মতো লাইব্রেরিগুলি প্রক্রিয়াটি সহজ করে তুলেছে।

বিকল্প? অবশ্যই। `wreq`, `req` এবং অন্যান্য আছে, প্রায়শই সিন্ট্যাক্টিক সুগার বা অতিরিক্ত ফিচার সহ। কিন্তু `http-client` আপনার ড্রয়ারে থাকা সেই নির্ভরযোগ্য সুইস আর্মি নাইফের মত, যেটি সবসময় কাজটি সারে।

অন্তর্নিহিতভাবে, `http-client` কানেকশন সামলাইতে একটি `Manager` ব্যবহার করে। এটি দক্ষ এবং সকেটগুলো পুনরায় ব্যবহার করে। আপনি এটি টিউন করতে পারেন, কিন্তু শুরুতে ডিফল্টগুলি ঠিক আছে।

## আরও দেখুন
আপনার টুলকিট প্রসারিত করতে, এগুলি দেখুন:

- [The `http-client` প্যাকেজ](https://www.stackage.org/package/http-client)
- [আরও আধুনিক দৃষ্টিকোণের `wreq` প্যাকেজ](https://www.stackage.org/package/wreq)
- [Haskell লাইব্রেরিগুলোর জন্য Hackage](https://hackage.haskell.org/)
