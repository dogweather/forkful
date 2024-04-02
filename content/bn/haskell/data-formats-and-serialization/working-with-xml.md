---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:15.270728-06:00
description: "\u09B9\u09BE\u09B8\u09CD\u0995\u09C7\u09B2\u09C7 XML \u09A8\u09BF\u09AF\
  \u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\
  \u09B2 XML \u0995\u09BE\u09A0\u09BE\u09AE\u09CB\u0997\u09C1\u09B2\u09BF \u09AA\u09BE\
  \u09B0\u09CD\u09B8 \u0995\u09B0\u09BE, \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\
  \u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09A4\u09C8\u09B0\
  \u09BF \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\
  \u09CD\u09AD\u09BF\u09B8 \u098F\u09AC\u0982 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\
  \u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2\u09B8\u09B9 \u09AF\u09C7 \u09B8\
  \u09AE\u09B8\u09CD\u09A4\u2026"
lastmod: '2024-03-17T18:47:44.109751-06:00'
model: gpt-4-0125-preview
summary: "\u09B9\u09BE\u09B8\u09CD\u0995\u09C7\u09B2\u09C7 XML \u09A8\u09BF\u09AF\u09BC\
  \u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\
  \ XML \u0995\u09BE\u09A0\u09BE\u09AE\u09CB\u0997\u09C1\u09B2\u09BF \u09AA\u09BE\u09B0\
  \u09CD\u09B8 \u0995\u09B0\u09BE, \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\
  \u09B2\u09C7\u099F \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\
  \u09AD\u09BF\u09B8 \u098F\u09AC\u0982 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\
  \u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2\u09B8\u09B9 \u09AF\u09C7 \u09B8\u09AE\
  \u09B8\u09CD\u09A4\u2026"
title: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 40
---

## কি ও কেন?

হাস্কেলে XML নিয়ে কাজ করা মানে হল XML কাঠামোগুলি পার্স করা, ম্যানিপুলেট করা এবং তৈরি করা। প্রোগ্রামাররা ওয়েব সার্ভিস এবং কনফিগারেশন ফাইলসহ যে সমস্ত অ্যাপ্লিকেশন এবং প্রোটোকল তাদের ডাটা ফর্ম্যাট হিসেবে XML ব্যবহার করে, তাদের সাথে ইন্টারএ্যাক্ট করার জন্য XML হ্যান্ডল করে থাকে।

## কিভাবে:

হাস্কেল XML নিয়ে কাজ করার জন্য `xml-conduit` এর মত লাইব্রেরিগুলো অফার করে। নিম্নে একটি XML স্ট্রিং পার্স করা এবং উপাদানগুলির জিজ্ঞাসাবাদ করা প্রদর্শিত হচ্ছে:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor

main :: IO ()
main = do
  let xmlContent = "<greetings><hello>World!</hello></greetings>"
  let document = parseLBS_ def $ T.encodeUtf8 $ T.pack xmlContent
  let cursor = fromDocument document

  let helloTexts = cursor $// element "hello" &/ content
  print helloTexts  -- ['World!']
```

নমুনা আউটপুট:

```
["World!"]
```

## গভীর ভাবে

XML, যা হল এক্সটেন্সিবল মার্কআপ ল্যাঙ্গুয়েজের সংক্ষিপ্ত রূপ, JSON-এর উত্থানের অনেক আগে থেকেই ডেটা সিরিয়ালাইজেশনে একটি মূল উপাদান হয়ে আসছে। এটি বাগাড়ম্বরপূর্ণ হলেও, কঠোর এবং মানকৃত, যা এটিকে কঠোর এন্টারপ্রাইজ পরিবেশ, লিগ্যাসি সিস্টেম এবং অর্থনীতি এবং স্বাস্থ্যসেবা মত শিল্পগুলোর জন্য উপযুক্ত করে তোলে।

হাস্কেলের XML প্রক্রিয়াকরণের জন্য বেশ কিছু লাইব্রেরি রয়েছে; তবে, `xml-conduit` তার কার্যকরী স্ট্রিমিং এবং পার্সিং ক্ষমতার জন্য সবচেয়ে শক্তিশালী এবং ব্যাপকভাবে ব্যবহৃত এর মধ্যে একটি, যা `conduit` পরিবারের অংশ যা ডেটা স্ট্রিমগুলি সম্পর্কে হ্যান্ডল করে।

বিকল্পগুলির মধ্যে `HXT` (Haskell XML Toolbox) রয়েছে যা পার্সিং এবং রূপান্তরে তীরচিহ্নগুলি (arrows) ব্যবহার করে, XML ম্যানিপুলেশনের জন্য একটি ভিন্ন আদর্শ প্রদান করে। যদিও `HXT` এর শেখার ঢাল তীব্র হওয়ায় এটি এখন কম জনপ্রিয়, তবুও কিছু ব্যবহারের ক্ষেত্রে এটি একটি সহজ পছন্দ হিসেবে রয়ে গেছে।

হাস্কেলে XML প্রক্রিয়াকরণ বাস্তবায়ন করার সময়, আপনাকে এনকোডিং সম্পর্কে সচেতন থাকতে হবে, কারণ হাস্কেল স্ট্রিংগুলি ইউনিকোড এবং XML ডেটা হতে পারে না। এছাড়াও, XML নেমস্পেসগুলি পার্সিং এ অতিরিক্ত জটিলতা যোগ করতে পারে।

## আরও দেখুন:

- `xml-conduit` প্যাকেজ ডকুমেন্টেশন: https://hackage.haskell.org/package/xml-conduit
- দ্য হাস্কেল XML টুলবক্স (HXT): http://hackage.haskell.org/package/hxt
- "রিয়েল ওয়ার্ল্ড হাস্কেল" বই, অধ্যায় 16, XML হ্যান্ডলিং জন্য: http://book.realworldhaskell.org/read/xml.html
- হাস্কেল উইকি অন XML: https://wiki.haskell.org/XML
