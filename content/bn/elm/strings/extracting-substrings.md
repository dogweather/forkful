---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:50.083054-06:00
description: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\
  \u09CD\u09B8\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u0995\u09CB\u09A8\u09CB \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u0985\u0982\u09B6 \u09AC\u09C7\u09B0 \u0995\u09B0\u09C7 \u0986\u09A8\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u099F\u09C7\
  \u0995\u09CD\u09B8\u099F \u09A1\u09C7\u099F\u09BE\u09B0 \u0985\u0982\u09B6\u0997\
  \u09C1\u09B2\u09BF\u0995\u09C7 \u09AA\u09C3\u09A5\u0995, \u09AE\u09CD\u09AF\u09BE\
  \u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE \u0985\u09A5\u09AC\
  \u09BE\u2026"
lastmod: '2024-03-17T18:47:43.937129-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\
  \u09CD\u09B8\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u0995\u09CB\u09A8\u09CB \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u0985\u0982\u09B6 \u09AC\u09C7\u09B0 \u0995\u09B0\u09C7 \u0986\u09A8\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u099F\u09C7\
  \u0995\u09CD\u09B8\u099F \u09A1\u09C7\u099F\u09BE\u09B0 \u0985\u0982\u09B6\u0997\
  \u09C1\u09B2\u09BF\u0995\u09C7 \u09AA\u09C3\u09A5\u0995, \u09AE\u09CD\u09AF\u09BE\
  \u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE \u0985\u09A5\u09AC\
  \u09BE\u2026"
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

সাবস্ট্রিং এক্সট্রাক্ট করা মানে কোনো স্ট্রিং থেকে নির্দিষ্ট অংশ বের করে আনা। প্রোগ্রামাররা এটি করে থাকেন টেক্সট ডেটার অংশগুলিকে পৃথক, ম্যানিপুলেট করা অথবা বিশ্লেষণ করার উদ্দেশ্যে।

## কিভাবে:

Elm এ এটি খুবই সহজ। শুরুতে, আমরা `String.slice` ব্যবহার করব:

```Elm
import String exposing (slice)

fullText : String
fullText = "Hello, Elm world!"

-- "Elm" এক্সট্রাক্ট করা
substring : String
substring = slice 7 10 fullText

-- আউটপুট: "Elm"
```

এবার, আমরা `String.left` এবং `String.right` এর সাথে একটু বেশি ডায়নামিক হব:

```Elm
import String exposing (left, right)

-- প্রথম 5 টি ক্যারেক্টার পেতে
leftString : String
leftString = left 5 fullText

-- আউটপুট: "Hello"

-- শেষের 5 টি ক্যারেক্টার পেতে
rightString : String
rightString = right 5 fullText

-- আউটপুট: "orld!"
```

## গভীর ডাইভ

ঐতিহাসিকভাবে, সাবস্ট্রিং এক্সট্রাকশন প্রোগ্রামিংয়ের প্রথম থেকেই রয়েছে। Elm এর মতো অন্যান্য ফাংশনাল ভাষাগুলিতে, স্ট্রিং ম্যানিপুলেশন ফাংশনগুলি ইমিউটেবল - তারা মূল স্ট্রিংটিকে পরিবর্তন করার পরিবর্তে নতুন স্ট্রিং ফিরিয়ে দেয়।

`String.dropLeft` এবং `String.dropRight` এর মতো বিকল্প ফাংশনগুলি অবশ্যই আছে। এগুলি স্ট্রিংয়ের এক প্রান্ত থেকে চরিত্রগুলিকে ট্রিম করে:

```Elm
import String exposing (dropLeft, dropRight)

-- প্রথম 7 টি ক্যারেক্টার বাদ দেওয়া
droppedLeftString : String
droppedLeftString = dropLeft 7 fullText

-- আউটপুট: "Elm world!"

-- শেষের 6 টি ক্যারেক্টার বাদ দেওয়া
droppedRightString : String
droppedRightString = dropRight 6 fullText

-- আউটপুট: "Hello, Elm"
```

বাস্তবায়নের দিক থেকে, এই ফাংশনগুলি Elm স্ট্যান্ডার্ড লাইব্রেরিতে অন্তর্ভুক্ত এবং এগুলি Unicode সমর্থন করে, যদিও Unicode-এর সুরোগেট জোড়া এবং কম্বাইনিং চরিত্রের সাথে কিছু বিবেচনা আছে।

## দেখুন

- Elm `String` মডিউল ডকুমেন্টেশন: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm গাইডে স্ট্রিংগুলি সম্পর্কে: https://guide.elm-lang.org/strings/
- MDN ওয়েব ডকস ওপর Unicode: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt
