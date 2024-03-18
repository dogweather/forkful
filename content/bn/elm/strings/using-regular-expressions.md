---
title:                "রেগুলার এক্সপ্রেশন ব্যবহার করা"
date:                  2024-03-17T18:26:28.667546-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
প্রোগ্রামিংয়ে নিয়মিত অভিব্যক্তি (regex) মূলত অক্ষরের সংমিশ্রণ মেলে দেখার জন্য ব্যবহৃত নিদর্শন। এলমে, অন্যান্য ভাষার মতো, প্রোগ্রামাররা regex ব্যবহার করে তথ্যের বৈধতা যাচাই, খোঁজা, এবং স্ট্রিংগুলিতে টেক্সট প্রতিস্থাপনের মতো কাজে তাদের নমনীয়তা এবং দক্ষতার জন্য।

## কিভাবে:
এলমের মূল লাইব্রেরিতে নিয়মিত অভিব্যক্তির জন্য নির্মিত ফাংশন নেই, এই অপারেশনগুলোর জন্য তৃতীয় পক্ষের লাইব্রেরিগুলি ব্যবহার করা প্রয়োজন। রেজেক্সের সাথে কাজ করার জন্য জনপ্রিয় একটি পছন্দ হল `elm/regex`। আপনি আপনার প্রজেক্টে `elm install elm/regex` ব্যবহার করে এটি যোগ করতে পারেন।

এখানে কিছু সাধারণ কাজের জন্য আপনি `elm/regex` কিভাবে ব্যবহার করতে পারেন:

### 1. একটি নিদর্শন মেলে দেখা
যদি একটি স্ট্রিং একটি নিদর্শনের সাথে মেলে কিনা তা যাচাই করতে, আপনি `Regex.contains` ব্যবহার করতে পারেন।

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- নমুনা ব্যবহার:
isAlphanumeric "Elm2023"     -- আউটপুট: True
isAlphanumeric "Elm 2023!"   -- আউটপুট: False
```

### 2. সমস্ত মিল খুঁজে বের করা
একটি স্ট্রিংয়ের মধ্যে একটি নিদর্শনের সব ঘটনা খুঁজে বের করতে, আপনি `Regex.find` ব্যবহার করতে পারেন।

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- নমুনা ব্যবহার:
getWords "Elm is fun!"  -- আউটপুট: ["Elm", "is", "fun"]
```

### 3. টেক্সট প্রতিস্থাপন
একটি স্ট্রিংয়ের যেসব অংশ একটি নিদর্শনের সাথে মেলে সেগুলি প্রতিস্থাপন করতে, আপনি `Regex.replace` ব্যবহার করতে পারেন।

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- নমুনা ব্যবহার:
replaceElmWithHaskell "Learning Elm is fun!"  
-- আউটপুট: "Learning Haskell is fun!"
```

এই উদাহরণগুলিতে, `Regex.fromString` ব্যবহার করে একটি regex নিদর্শন কম্পাইল করা হয়, যেখানে `\b` শব্দের সীমানাগুলির সাথে মেলে, এবং `\w` যেকোনো শব্দ অক্ষরের সাথে মেলে। `Regex.fromString`-এর `Maybe` ফলাফলটি সবসময় সামলানো উচিত, অবৈধ regex নিদর্শনের বিরুদ্ধে নিরাপদ থাকার জন্য, সাধারণত `Maybe.withDefault` ব্যবহার করে।
