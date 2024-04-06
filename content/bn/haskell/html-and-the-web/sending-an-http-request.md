---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:17:34.377626-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8 \u09AE\
  \u099C\u09BE\u09B0 \u09AC\u09BF\u09B7\u09AF\u09BC\u09C7 \u099A\u09B2\u09C7 \u09AF\
  \u09BE\u0987\u0964 \u0986\u09AA\u09A8\u09BE\u09B0 `http-client` \u098F\u09AC\u0982\
  \ `http-client-tls` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AA\u09CD\u09B0\
  \u09AF\u09BC\u09CB\u099C\u09A8 \u09B9\u09AC\u09C7\u0964 \u0986\u09AA\u09A8\u09BE\
  \u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u0995 \u09B8\u09C7\u099F \u0995\u09B0\
  \u09C1\u09A8 \u098F\u09AC\u0982 \u098F\u0997\u09C1\u09B2\u09CB\u0995\u09C7 \u0986\
  \u09AA\u09A8\u09BE\u09B0 `package.yaml`\u2026"
lastmod: '2024-04-05T21:53:52.462397-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8 \u09AE\u099C\u09BE\u09B0 \u09AC\u09BF\u09B7\u09AF\
  \u09BC\u09C7 \u099A\u09B2\u09C7 \u09AF\u09BE\u0987\u0964 \u0986\u09AA\u09A8\u09BE\
  \u09B0 `http-client` \u098F\u09AC\u0982 `http-client-tls` \u09AA\u09CD\u09AF\u09BE\
  \u0995\u09C7\u099C \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09B9\u09AC\
  \u09C7\u0964 \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\
  \u0995 \u09B8\u09C7\u099F \u0995\u09B0\u09C1\u09A8 \u098F\u09AC\u0982 \u098F\u0997\
  \u09C1\u09B2\u09CB\u0995\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 `package.yaml` \u0985\
  \u09A5\u09AC\u09BE `.cabal` \u09AB\u09BE\u0987\u09B2\u09C7 \u09AF\u09CB\u0997 \u0995\
  \u09B0\u09C1\u09A8\u0964 \u09A4\u09BE\u09B0\u09AA\u09B0, `stack build` \u09AC\u09BE\
  \ \u0989\u09AA\u09AF\u09C1\u0995\u09CD\u09A4 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1\
  \ \u099A\u09BE\u09B2\u09BE\u09A8 \u09A4\u09BE\u09A6\u09C7\u09B0 \u0985\u09A8\u09C1\
  \u09B8\u09A8\u09CD\u09A7\u09BE\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\u0964\
  \ \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09BF\u09AE\u09CD\
  \u09AA\u09B2 GET \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09B0\u09AF\u09BC\u09C7\u099B\
  \u09C7."
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

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
