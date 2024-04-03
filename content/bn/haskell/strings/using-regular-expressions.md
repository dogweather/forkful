---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:00.670059-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Haskell \u098F, regex \u09AB\u09BE\
  \u0982\u09B6\u09A8\u09BE\u09B2\u09BF\u099F\u09BF\u0997\u09C1\u09B2\u09BF \u09AE\u09BE\
  \u09A8\u0995 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u0985\
  \u0982\u09B6 \u09A8\u09AF\u09BC, \u09AF\u09BE\u09B0 \u09AB\u09B2\u09C7 `regex-base`\
  \ \u098F\u09B0 \u09AE\u09A4\u09CB \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\
  \u09CD\u09B7\u09C7\u09B0 \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u09C7\u09B0\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u098F\u09AC\u0982 \u098F\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09B8\u09BE\u09AE\u099E\u09CD\u099C\u09B8\u09CD\u09AF\u09AA\u09C2\
  \u09B0\u09CD\u09A3\u2026"
lastmod: '2024-03-17T18:47:44.071372-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u098F, regex \u09AB\u09BE\u0982\u09B6\u09A8\u09BE\u09B2\u09BF\u099F\
  \u09BF\u0997\u09C1\u09B2\u09BF \u09AE\u09BE\u09A8\u0995 \u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u0985\u0982\u09B6 \u09A8\u09AF\u09BC, \u09AF\
  \u09BE\u09B0 \u09AB\u09B2\u09C7 `regex-base` \u098F\u09B0 \u09AE\u09A4\u09CB \u09A4\
  \u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09AA\u09CD\
  \u09AF\u09BE\u0995\u09C7\u099C\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u098F\u09AC\u0982 \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B8\u09BE\u09AE\u099E\
  \u09CD\u099C\u09B8\u09CD\u09AF\u09AA\u09C2\u09B0\u09CD\u09A3 \u09AC\u09CD\u09AF\u09BE\
  \u0995\u098F\u09A8\u09CD\u09A1 \u09AF\u09C7\u09AE\u09A8 `regex-posix` (POSIX regex\
  \ \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF), `regex-pcre`\
  \ (Perl-\u09B8\u09BE\u09AE\u099E\u09CD\u099C\u09B8\u09CD\u09AF\u09AA\u09C2\u09B0\
  \u09CD\u09A3 regex \u098F\u09B0 \u099C\u09A8\u09CD\u09AF), \u0987\u09A4\u09CD\u09AF\
  \u09BE\u09A6\u09BF\u09B0 \u09A6\u09B0\u0995\u09BE\u09B0 \u09B9\u09AF\u09BC\u0964\
  \ \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\u09A4 \u0985\u09AD\u09BF\u09AC\u09CD\u09AF\
  \u0995\u09CD\u09A4\u09BF\u0997\u09C1\u09B2\u09BF \u09B8\u09BE\u09A5\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0986\u09AA\u09A8\
  \u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u098F\u0987 \u09AA\u09CD\u09AF\u09BE\
  \u0995\u09C7\u099C\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8 \u09A4\u09BE \u098F\u0996\
  \u09BE\u09A8\u09C7\u0964\n\n\u09AA\u09CD\u09B0\u09A5\u09AE\u09A4, \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA\u09C7\u09B0 `.cabal` \u09AB\
  \u09BE\u0987\u09B2\u09C7 `regex-posix` \u09AC\u09BE `regex-pcre` \u09AF\u09CB\u0997\
  \ \u0995\u09B0\u09C7 \u09AC\u09BE \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF cabal \u098F\
  \u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u0987\u09A8\u09B8\u09CD\u099F\
  \u09B2 \u0995\u09B0\u09C7 \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\
  \u09C1\u09A8 \u09AF\u09C7 \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u0997\u09C1\
  \u09B2\u09BF \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09BE \u0986\u099B\
  \u09C7."
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

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
