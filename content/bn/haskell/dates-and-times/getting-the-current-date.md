---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:29.302860-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Haskell \u098F\u09B0 \u09AE\u09BE\
  \u09A8\u0995 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF, `base`, \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982 \u09B8\u09AE\u09DF \u09A8\u09BF\u09DF\
  \u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF `Data.Time`\
  \ \u09AE\u09A1\u09BF\u0989\u09B2 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\
  \u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB\
  \ \u09B9\u09B2\u09CB \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u098F\u099F\u09BF \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09AC\u09B0\u09CD\u09A4\
  \u09AE\u09BE\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.094799-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u098F\u09B0 \u09AE\u09BE\u09A8\u0995 \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF, `base`, \u09A4\u09BE\u09B0\u09BF\u0996 \u098F\u09AC\u0982\
  \ \u09B8\u09AE\u09DF \u09A8\u09BF\u09DF\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF `Data.Time` \u09AE\u09A1\u09BF\u0989\u09B2 \u09AA\
  \u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2\u09CB \u0995\u09BF\u09AD\
  \u09BE\u09AC\u09C7 \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 \u09B9\u09DF."
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কিভাবে:
Haskell এর মানক লাইব্রেরি, `base`, তারিখ এবং সময় নিয়ে কাজ করার জন্য `Data.Time` মডিউল প্রদান করে। এখানে দেখানো হলো কিভাবে এটি ব্যবহার করে বর্তমান তারিখ পেতে হয়:

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

নমুনা আউটপুট:
```
2023-04-12
```

তারিখ ফর্ম্যাট করা বা ভিন্ন সময় অঞ্চল নিয়ে কাজ করার মতো আরও নমনীয়তা প্রয়োজন হলে, `time` লাইব্রেরি অপরিহার্য। এখানে দেখানো হলো কিভাবে আপনি বর্তমান তারিখ ফর্ম্যাট করতে পারেন:

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

এই কোড বর্তমান তারিখকে `YYYY-MM-DD` ফর্ম্যাটে প্রিন্ট করে, যা স্থানীয় সময় অঞ্চলের সাথে সমন্বয় করা হয়েছে।

তৃতীয়-পক্ষের লাইব্রেরি সমর্থনের জন্য, `time` উচ্চভাবে সুপারিশ করা হয় এবং Haskell সম্প্রদায়ের মধ্যে তারিখ এবং সময় পরিচালনার বিপুল ক্ষমতায় প্রায়শই ব্যবহার করা হয়। উপরের উদাহরণগুলি এই লাইব্রেরিটি ব্যবহার করে।

যদি আপনার তারিখের সাথে আরও ব্যাপক পরিচালন, যেমন স্ট্রিং থেকে পার্সিং বা তারিখ এবং সময়ের সাথে অংকের অপারেশনগুলি প্রয়োজন হয়, তাহলে `Data.Time` এর অতিরিক্ত ফাংশনগুলি অন্বেষণ করা উপকারী হবে।
