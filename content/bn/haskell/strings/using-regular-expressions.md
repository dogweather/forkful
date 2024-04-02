---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:00.670059-06:00
description: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\u09A4 \u0985\u09AD\u09BF\u09AC\
  \u09CD\u09AF\u0995\u09CD\u09A4\u09BF\u0997\u09C1\u09B2\u09BF \u09B9\u09B2 \u0985\
  \u0995\u09CD\u09B7\u09B0\u09C7\u09B0 \u0985\u09A8\u09C1\u0995\u09CD\u09B0\u09AE\
  \ \u09AF\u09BE \u098F\u0995\u099F\u09BF \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u09A8\u09BF\
  \u09B0\u09CD\u09A7\u09BE\u09B0\u09A3 \u0995\u09B0\u09C7, \u09B8\u09BE\u09A7\u09BE\
  \u09B0\u09A3\u09A4 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09B8\u09BE\u09B0\
  \u09CD\u099A\u09BF\u0982 \u098F\u09AC\u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\
  \u09AA\u09C1\u09B2\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09C3\u09A4\u2026"
lastmod: '2024-03-17T18:47:44.071372-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\u09A4 \u0985\u09AD\u09BF\u09AC\
  \u09CD\u09AF\u0995\u09CD\u09A4\u09BF\u0997\u09C1\u09B2\u09BF \u09B9\u09B2 \u0985\
  \u0995\u09CD\u09B7\u09B0\u09C7\u09B0 \u0985\u09A8\u09C1\u0995\u09CD\u09B0\u09AE\
  \ \u09AF\u09BE \u098F\u0995\u099F\u09BF \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u09A8\u09BF\
  \u09B0\u09CD\u09A7\u09BE\u09B0\u09A3 \u0995\u09B0\u09C7, \u09B8\u09BE\u09A7\u09BE\
  \u09B0\u09A3\u09A4 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09B8\u09BE\u09B0\
  \u09CD\u099A\u09BF\u0982 \u098F\u09AC\u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\
  \u09AA\u09C1\u09B2\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09C3\u09A4\u2026"
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

## কি এবং কেন?
প্রোগ্রামিংয়ে নিয়মিত অভিব্যক্তিগুলি হল অক্ষরের অনুক্রম যা একটি অনুসন্ধান প্যাটার্ন নির্ধারণ করে, সাধারণত স্ট্রিং সার্চিং এবং ম্যানিপুলেশনের জন্য ব্যবহৃত হয়। Haskell প্রোগ্রামারগণ সরল স্ট্রিং মিল থেকে জটিল টেক্সট প্রসেসিং কাজে নিয়মিত অভিব্যক্তিগুলি ব্যবহার করে, টেক্সট ডাটা নিয়ে কাজ করার ক্ষেত্রে তাদের দক্ষতা এবং বহুমুখিনতা এর সুবিধা গ্রহণ করে।

## কিভাবে:
Haskell এ, regex ফাংশনালিটিগুলি মানক লাইব্রেরির অংশ নয়, যার ফলে `regex-base` এর মতো তৃতীয় পক্ষের প্যাকেজের ব্যবহার এবং এর সাথে সামঞ্জস্যপূর্ণ ব্যাকএন্ড যেমন `regex-posix` (POSIX regex সমর্থনের জন্য), `regex-pcre` (Perl-সামঞ্জস্যপূর্ণ regex এর জন্য), ইত্যাদির দরকার হয়। নিয়মিত অভিব্যক্তিগুলি সাথে কাজ করার জন্য আপনি কিভাবে এই প্যাকেজগুলি ব্যবহার করতে পারেন তা এখানে।

প্রথমত, আপনার প্রকল্পের `.cabal` ফাইলে `regex-posix` বা `regex-pcre` যোগ করে বা সরাসরি cabal এর মাধ্যমে ইনস্টল করে নিশ্চিত করুন যে প্যাকেজগুলি ইনস্টল করা আছে:

```bash
cabal install regex-posix
```
অথবা
```bash
cabal install regex-pcre
```

### `regex-posix` ব্যবহার করে:

```haskell
import Text.Regex.Posix ((=~))

-- একটি স্ট্রিং একটি প্যাটার্নের সাথে মিলে কিনা তা চেক করা
isMatch :: String -> String -> Bool
isMatch text pattern = text =~ pattern :: Bool

-- প্রথম মিল খুঁজে পাওয়া
findFirst :: String -> String -> String
findFirst text pattern = text =~ pattern :: String

main :: IO ()
main = do
    print $ isMatch "hello world" "wo"
    -- আউটপুট: True
    print $ findFirst "good morning, good night" "good"
    -- আউটপুট: "good"
```

### `regex-pcre` ব্যবহার করে:

```haskell
import Text.Regex.PCRE ((=~))

-- সকল মিল খুঁজে পাওয়া
findAll :: String -> String -> [String]
findAll text pattern = text =~ pattern :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- আউটপুট: ["test1","test2","test3"]
```

প্রতিটি লাইব্রেরির নিজস্ব অঙ্গীকার থাকলেও, মিল চেক করা বা উপস্ট্রিং বের করার জন্য `=~` ব্যবহার করার সাধারণ পদ্ধতিটি অভিন্ন থাকে। আপনার প্রকল্পের চাহিদা এবং নির্দিষ্ট regex সামর্থ্যের উপর ভিত্তি করে `regex-posix` বা `regex-pcre` এর মধ্যে নির্বাচন করা হয়।
