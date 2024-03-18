---
title:                "রেগুলার এক্সপ্রেশন ব্যবহার করা"
date:                  2024-03-17T18:27:00.670059-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
