---
title:                "স্ট্রিং ইন্টারপোলেট করা"
date:                  2024-03-17T17:50:44.779294-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
