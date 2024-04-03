---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:04:03.534259-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Haskell \u098F HTML \u09AA\u09BE\
  \u09B0\u09CD\u09B8 \u0995\u09B0\u09A4\u09C7, \u0986\u09AE\u09B0\u09BE \u098F\u09B0\
  \ \u09B8\u09BE\u09A6\u09BE\u09B8\u09BF\u09A7\u09BE \u098F\u09AC\u0982 \u09A8\u09AE\
  \u09A8\u09C0\u09AF\u09BC\u09A4\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF `tagsoup` \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09AC\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u0986\u09AA\
  \u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u099C\u09C7\u0995\u09CD\u099F\u09C7\u09B0\
  \ cabal \u09AB\u09BE\u0987\u09B2\u09C7 `tagsoup`\u2026"
lastmod: '2024-03-17T18:47:44.080246-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u098F HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09A4\u09C7\
  , \u0986\u09AE\u09B0\u09BE \u098F\u09B0 \u09B8\u09BE\u09A6\u09BE\u09B8\u09BF\u09A7\
  \u09BE \u098F\u09AC\u0982 \u09A8\u09AE\u09A8\u09C0\u09AF\u09BC\u09A4\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF `tagsoup` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09AC\u0964 \u09AA\
  \u09CD\u09B0\u09A5\u09AE\u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\
  \u099C\u09C7\u0995\u09CD\u099F\u09C7\u09B0 cabal \u09AB\u09BE\u0987\u09B2\u09C7\
  \ `tagsoup` \u09AF\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09C7 \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u099F\u09BF \u0987\u09A8\u09B8\u09CD\u099F\u09B2\
  \ \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AE\u09A8\u09C7 \u09B0\u09BE\
  \u0996\u09AC\u09C7\u09A8 \u0985\u09A5\u09AC\u09BE `cabal install tagsoup` \u099A\
  \u09BE\u09B2\u09BE\u09A8\u0964."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কিভাবে:
Haskell এ HTML পার্স করতে, আমরা এর সাদাসিধা এবং নমনীয়তার জন্য `tagsoup` লাইব্রেরি ব্যবহার করব। প্রথমে, আপনার প্রজেক্টের cabal ফাইলে `tagsoup` যুক্ত করে লাইব্রেরিটি ইনস্টল করার জন্য মনে রাখবেন অথবা `cabal install tagsoup` চালান। 

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup

-- ডেমোনস্ট্রেশনের জন্য স্যাম্পল HTML
let sampleHtml = "<html><body><p>Learn Haskell!</p><a href='http://example.com'>Click Here</a></body></html>"

-- HTML পার্স করে লিংকগুলি (a ট্যাগ) ফিল্টার করুন
let tags = parseTags sampleHtml
let links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- এক্সট্র্যাক্ট করা লিংকগুলি প্রিন্ট করুন
print links
```

স্যাম্পল আউটপুট:
```plaintext
["http://example.com"]
```

আরও জটিল HTML পার্সিং প্রয়োজনের জন্য, বিশেষ করে ডকুমেন্ট রূপান্তরের সাথে কাজ করলে, `pandoc` লাইব্রেরি বিবেচনা করুন। এটি অসামান্য বহুমুখী, কিন্তু এর জটিলতা বেশি:

```haskell
import Text.Pandoc

-- ধরে নিচ্ছি আপনার কাছে একটি Pandoc ডকুমেন্ট (doc) লোড করা আছে, যেমন, একটি ফাইল পড়ে
let doc = ... -- আপনার Pandoc ডকুমেন্ট এখানে যাবে

-- ডকুমেন্টকে HTML স্ট্রিং-এ রূপান্তর করুন
let htmlString = writeHtmlString def doc

-- এখন, আপনি `htmlString` উপরের মত পার্স করবেন অথবা আপনার প্রয়োজন অনুযায়ী এগিয়ে যাবেন।
```
`pandoc` হল একটি বৃহত্তর লাইব্রেরি যা বিভিন্ন মার্কআপ ফর্ম্যাটের মধ্যে রূপান্তরে মনোনিবেশ করে, তাই যদি আপনার সেই অতিরিক্ত ক্ষমতা প্রয়োজন হয় অথবা আপনার অ্যাপ্লিকেশনে ইতিমধ্যে ডকুমেন্ট ফর্ম্যাটের সাথে কাজ করছেন, তবে এটি ব্যবহার করুন।
