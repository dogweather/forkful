---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:30.667163-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Lua-\u09A4\u09C7 \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\
  \u0995\u09CD\u09B7\u09B0\u09C7 \u09AA\u09CD\u09B0\u0995\u09BE\u09B6\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09AB\u09BE\
  \u0982\u09B6\u09A8 \u09A8\u09C7\u0987, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u0986\
  \u09AA\u09A8\u09BF \u09AC\u09C7\u09B8\u09BF\u0995 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8\
  \ \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0987 \u0995\u09BE\u099C\u099F\u09BF\
  \ \u09B8\u09B9\u099C\u09C7\u0987 \u0995\u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.159558-06:00'
model: gpt-4-0125-preview
summary: "Lua-\u09A4\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09A1\
  \u09BC \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7 \u09AA\
  \u09CD\u09B0\u0995\u09BE\u09B6\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\
  \u09B2\u09CD\u099F-\u0987\u09A8 \u09AB\u09BE\u0982\u09B6\u09A8 \u09A8\u09C7\u0987\
  , \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u0986\u09AA\u09A8\u09BF \u09AC\u09C7\u09B8\
  \u09BF\u0995 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AE\u09CD\u09AF\u09BE\
  \u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\
  \u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u098F\u0987 \u0995\u09BE\u099C\u099F\u09BF \u09B8\u09B9\u099C\u09C7\u0987 \u0995\
  \u09B0\u09C7 \u09AB\u09C7\u09B2\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964\
  \ \u098F\u0995\u099F\u09BF \u098F\u0995\u0995 \u09B6\u09AC\u09CD\u09A6\u09C7\u09B0\
  \ \u09AA\u09CD\u09B0\u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC\
  \ \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7 \u09AA\u09CD\
  \u09B0\u0995\u09BE\u09B6\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\
  \u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u09AB\u09BE\u0982\u09B6\u09A8 \u098F\
  \u0996\u09BE\u09A8\u09C7 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
weight: 2
---

## কিভাবে:
Lua-তে স্ট্রিং বড় হাতের অক্ষরে প্রকাশের জন্য বিল্ট-ইন ফাংশন নেই, কিন্তু আপনি বেসিক স্ট্রিং ম্যানিপুলেশন ফাংশনগুলি ব্যবহার করে এই কাজটি সহজেই করে ফেলতে পারেন। একটি একক শব্দের প্রথম অক্ষর বড় হাতের অক্ষরে প্রকাশের জন্য একটি সাধারণ ফাংশন এখানে দেওয়া হল:

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- আউটপুট: Hello
```

একটি বাক্যে প্রতিটি শব্দের প্রথম অক্ষরকে বড় হাতের অক্ষরে প্রকাশ করতে, আপনি বাক্যটিকে শব্দে শব্দে ভাগ করে, প্রতিটি একটিকে বড় হাতের অক্ষরে পরিবর্তন করে, এবং তারপর তাদের আবার জোড়া দিতে পারেন:

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- আউটপুট: Hello World From Lua
```

যদি আপনি এমন একটি প্রজেক্টে কাজ করছেন যেখানে পারফরমেন্স মুখ্য এবং আপনার আরও উন্নত স্ট্রিং ম্যানিপুলেশন ক্ষমতা প্রয়োজন, `Penlight` মতো থার্ড-পার্টি লাইব্রেরিটি বিবেচনা করুন। Penlight লুয়াকে আরও বহুমুখী স্ট্রিং হ্যান্ডলিং ফাংশনের মধ্যে অন্যান্য উপযোগিতা সহ বাড়িয়ে দেয়:

```lua
-- ধরে নেওয়া হচ্ছে Penlight ইনস্টল করা আছে:
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- আউটপুট: Hello lua users

-- লক্ষ্য করুন: Penlight-এর capitalized ফাংশন শুধুমাত্র প্রথম শব্দটিকেই বড় হাতের অক্ষরে প্রকাশ করে।
-- প্রতিটি শব্দকে বড় হাতের অক্ষরে প্রকাশ করতে, আপনার একটি স্বনির্ধারিত সমাধান বা অন্যান্য লাইব্রেরিগুলি অনুসন্ধান করতে হবে।
```
