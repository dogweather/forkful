---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:25.044946-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AF\u09C1\u0995\u09CD\u09A4\
  \ \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u09C7 \u0985\u09AA\u09B0\
  \u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B6\u09C7\u09B7 \u09AA\u09CD\u09B0\u09BE\
  \u09A8\u09CD\u09A4 \u09A5\u09C7\u0995\u09C7 \u09B6\u09C7\u09B7 \u09AA\u09CD\u09B0\
  \u09BE\u09A8\u09CD\u09A4 \u09AA\u09B0\u09CD\u09AF\u09A8\u09CD\u09A4 \u099C\u09CB\
  \u09A1\u09BC\u09BE \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09AF\u09BE\u09A4\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09A8\u09A4\u09C1\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09A4\u09C8\u09B0\u09BF \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u099F\u09C7\u0995\u09CD\
  \u09B8\u099F\u0995\u09C7 \u0997\u09A4\u09BF\u09B6\u09C0\u09B2\u09AD\u09BE\u09AC\u09C7\
  \u2026"
lastmod: '2024-03-17T18:47:44.170603-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AF\u09C1\u0995\u09CD\u09A4\
  \ \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u09C7 \u0985\u09AA\u09B0\
  \u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B6\u09C7\u09B7 \u09AA\u09CD\u09B0\u09BE\
  \u09A8\u09CD\u09A4 \u09A5\u09C7\u0995\u09C7 \u09B6\u09C7\u09B7 \u09AA\u09CD\u09B0\
  \u09BE\u09A8\u09CD\u09A4 \u09AA\u09B0\u09CD\u09AF\u09A8\u09CD\u09A4 \u099C\u09CB\
  \u09A1\u09BC\u09BE \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09AF\u09BE\u09A4\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09A8\u09A4\u09C1\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09A4\u09C8\u09B0\u09BF \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u099F\u09C7\u0995\u09CD\
  \u09B8\u099F\u0995\u09C7 \u0997\u09A4\u09BF\u09B6\u09C0\u09B2\u09AD\u09BE\u09AC\u09C7\
  \ \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u09AF\
  \u09C7\u09AE\u09A8 \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09BE \u09AC\u09BE \u0995\u09CB\u09A1 \u099C\u09C7\u09A8\u09BE\u09B0\
  \u09C7\u099F \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
weight: 3
---

## কিভাবে:
Lua-তে, আপনি `..` অপারেটর ব্যবহার করে স্ট্রিং যোগ করতে পারেন। এটি কিভাবে কাজ করে তা দেখা যাক:

```lua
local hello = "Hello, "
local world = "world!"
local greeting = hello .. world

print(greeting)  -- আউটপুট: Hello, world!
```

আপনি এমনকি সংখ্যাগুলিকেও একটু জোরাজুরি করে যোগ করতে পারেন:

```lua
local base = "I have "
local itemCount = 3
local message = base .. itemCount .. " apples"

print(message)  -- আউটপুট: I have 3 apples
```

মনে রাখবেন, স্ট্রিং ছাড়া অন্যান্য ধরনের রূপান্তর ম্যানুয়ালি করতে হবে:

```lua
local score = 9001
local displayScore = "Your score is: " .. tostring(score)

print(displayScore)  -- আউটপুট: Your score is: 9001
```

## গভীর ডুব
স্ট্রিং যোগ হয়তো সাধারণ মনে হতে পারে, কিন্তু এটি গুরুত্বপূর্ণ। Lua-র প্রাথমিক দিনগুলিতে, এটি এম্বেডেড সিস্টেমের জন্য প্রস্তাবিত ছিল, যা মানে সবকিছুকে হালকা রাখা। তাই `..` স্ট্রিংগুলির জন্য নির্বাচিত হয়েছিল - এটি সাধারণ কিন্তু কার্যকর।

`..` এর বিকল্প অন্তর্ভুক্ত:

- `table.concat` ফাংশন স্ট্রিং অ্যারের জন্য, অনেক স্ট্রিং যোগ করার জন্য আরও কার্যকর।
- `string.format` এর মতো স্ট্রিং লাইব্রেরি ফাংশন, বিন্যাস নিয়ন্ত্রণে অধিক নিয়ন্ত্রণ প্রদান করে।

Lua-তে স্ট্রিং যোগের কার্যকারিতা উদ্বেগের বিষয় ছিল, বিশেষ করে `..` এর সাথে কারণ প্রতিটি ব্যবহারে একটি নতুন স্ট্রিং তৈরি হয়, যা লুপে ব্যয়বহুল হতে পারে। এটি কাটিয়ে ওঠার জন্য, লুপে যোগ করার সময়, টেবিল ব্যবহার করুন:

```lua
local parts = {}
for i = 1, 10 do
    parts[i] = "Part " .. i
end
local combined = table.concat(parts, ", ")

print(combined)  -- আউটপুট: Part 1, Part 2, ... Part 10
```

অভ্যন্তরীণভাবে, Lua স্মৃতি ব্যবহার অপ্টিমাইজ করার জন্য হ্যাশ টেবিলে স্ট্রিংগুলি পরিচালনা করে, সুতরাং একই স্ট্রিংগুলি একই স্টোরেজ ভাগাভাগি করে। কিন্তু, নতুন স্ট্রিং তৈরির কারণে যোগ করা এই ভাগাভাগিকে ভাঙ্গে।

## দেখুন
- Lua-র অফিসিয়াল ডকুমেন্টেশন স্ট্রিং সম্পর্কিত: https://www.lua.org/manual/5.4/manual.html#6.4
- প্রোগ্রামিং ইন Lua (বই): https://www.lua.org/pil/contents.html
- স্ট্রিং ম্যানিপুলেশন টিপস: https://lua-users.org/wiki/StringLibraryTutorial
