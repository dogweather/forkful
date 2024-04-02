---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:44.779294-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u0986\u09AA\u09A8\u09BE\u0995\u09C7\
  \ \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\
  \u09B2 \u098F\u09AE\u09AC\u09C7\u09A1 \u0995\u09B0\u09A4\u09C7 \u09A6\u09C7\u09AF\
  \u09BC\u0964 \u098F\u099F\u09BF \u09B8\u09C1\u09AC\u09BF\u09A7\u09BE \u098F\u09AC\
  \u0982 \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF\u09A4\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\u2014\u0986\u09AA\u09A8\
  \u09BE\u09B0 \u09AE\u09C7\u09B8\u09C7\u099C \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\
  \u09A4\u09C7 \u0995\u09CB\u09A8\u09CB \u09AA\u09CD\u09B2\u09BE\u09B8 \u099A\u09BF\
  \u09B9\u09CD\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.067290-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u0986\u09AA\u09A8\u09BE\u0995\u09C7\
  \ \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\
  \u09B2 \u098F\u09AE\u09AC\u09C7\u09A1 \u0995\u09B0\u09A4\u09C7 \u09A6\u09C7\u09AF\
  \u09BC\u0964 \u098F\u099F\u09BF \u09B8\u09C1\u09AC\u09BF\u09A7\u09BE \u098F\u09AC\
  \u0982 \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF\u09A4\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\u2014\u0986\u09AA\u09A8\
  \u09BE\u09B0 \u09AE\u09C7\u09B8\u09C7\u099C \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\
  \u09A4\u09C7 \u0995\u09CB\u09A8\u09CB \u09AA\u09CD\u09B2\u09BE\u09B8 \u099A\u09BF\
  \u09B9\u09CD\u09A8\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কি এবং কেন?

স্ট্রিং ইন্টারপোলেশন আপনাকে সরাসরি স্ট্রিংগুলিতে ভেরিয়েবল এমবেড করতে দেয়। এটি সুবিধা এবং পাঠযোগ্যতার জন্য করা হয়—আপনার মেসেজ তৈরি করতে কোনো প্লাস চিহ্ন বা ফাংশন কল প্রয়োজন হয় না।

## কিভাবে:

হাস্কেলে, স্ট্রিং ইন্টারপোলেশন অন্তর্ভুক্ত নয়, তবে `interpolate` প্যাকেজের সাহায্যে, আপনি প্রায় একই কাজ করতে পারেন। প্রথমে, নিশ্চিত করুন আপনার কাছে প্যাকেজটি আছে:

```bash
cabal update
cabal install interpolate
```

এখন, কিছু হাস্কেল লিখুন:

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate (i)

main :: IO ()
main = do
    let name = "world"
    let greeting = [i|Hello, #{name}!|]
    putStrLn greeting
```

এটি চালান:

```
Hello, world!
```

## গভীরে গমন

ঐতিহাসিকভাবে, হাস্কেলে স্ট্রিং ইন্টারপোলেশন বক্সের বাইরে আসেনি। এটি স্ক্রিপ্টিং ভাষায় আরও সাধারণ একটি বৈশিষ্ট্য। হাস্কেলে ইন্টারপোলেশন কোয়াসিকোটারসের বিকাশের সাথে মসৃণ হয়েছে, যা আপনার নিজের কাস্টম সিনট্যাক্স ডিফাইন করতে দেয়—যেমন আমাদের `i` স্ট্রিং ইন্টারপোলেট করার জন্য।

বিকল্প? অবশ্যই, `Text.Printf` থেকে `printf` ব্যবহার করুন, অথবা `++` এর সাথে স্ট্রিং এবং ভেরিয়েবল জোড়া। কিন্তু এগুলি ইন্টারপোলেশনের সৌন্দর্য এবং সারল্যের অভাব রাখে।

বাস্তবায়নের দিক থেকে, `interpolate` আপনার ইন্টারপোলেট করা স্ট্রিংগুলিকে কম্পাইল-টাইমে নিয়মিত হাস্কেল স্ট্রিং-এ পরিণত করে Template Haskell ব্যবহার করে, তাই আপনার কোড চালানোর সময় কোনো পারফরমেন্স হিটের মুখোমুখি হয় না। এটি চালাক এবং পরিষ্কার, ঠিক হাস্কেলের মতো।

## আরও দেখুন

- [Hackage - interpolate প্যাকেজ](https://hackage.haskell.org/package/interpolate)
- [Hackage - Text.Printf মডিউল](https://hackage.haskell.org/package/base/docs/Text-Printf.html)
- [Haskell Wiki - Quasiquotation](https://wiki.haskell.org/Quasiquotation)
- সমৃদ্ধ টেমপ্লেটিং এর জন্য, [Hackage - Mustache templates](https://hackage.haskell.org/package/mustache) দেখুন
