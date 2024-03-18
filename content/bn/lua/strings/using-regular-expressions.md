---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:02.462433-06:00
description: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\u09A4 \u098F\u0995\u09CD\u09B8\
  \u09AA\u09CD\u09B0\u09C7\u09B6\u09A8 (Regular expressions) \u09AC\u09BF\u09B6\u09C7\
  \u09B7 \u09A7\u09B0\u09A8\u09C7\u09B0 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\
  \u09CD\u09A8\u0995\u09C7 \u09AE\u09BF\u09B2\u09BF\u09DF\u09C7 \u098F\u09AC\u0982\
  \ \u09B8\u09C7\u0987 \u0985\u09A8\u09C1\u09AF\u09BE\u09DF\u09C0 \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8 \u099A\u09BE\u09B2\u09BE\u09A8\u09CB\
  \u09B0 \u0985\u09A8\u09C1\u09AE\u09A4\u09BF \u09A6\u09C7\u09AF\u09BC\u0964\u2026"
lastmod: '2024-03-17T18:47:44.168065-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\u09A4 \u098F\u0995\u09CD\u09B8\
  \u09AA\u09CD\u09B0\u09C7\u09B6\u09A8 (Regular expressions) \u09AC\u09BF\u09B6\u09C7\
  \u09B7 \u09A7\u09B0\u09A8\u09C7\u09B0 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\
  \u09CD\u09A8\u0995\u09C7 \u09AE\u09BF\u09B2\u09BF\u09DF\u09C7 \u098F\u09AC\u0982\
  \ \u09B8\u09C7\u0987 \u0985\u09A8\u09C1\u09AF\u09BE\u09DF\u09C0 \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8 \u099A\u09BE\u09B2\u09BE\u09A8\u09CB\
  \u09B0 \u0985\u09A8\u09C1\u09AE\u09A4\u09BF \u09A6\u09C7\u09AF\u09BC\u0964\u2026"
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

প্রোগ্রামিংয়ে নিয়মিত এক্সপ্রেশন (Regular expressions) বিশেষ ধরনের প্যাটার্নকে মিলিয়ে এবং সেই অনুযায়ী স্ট্রিংগুলির সাথে অপারেশন চালানোর অনুমতি দেয়। প্রোগ্রামাররা ভ্যালিডেশন, সার্চিং, এবং টেক্সট ম্যানিপুলেশনের মতো কাজের জন্য তাদের ব্যবহার করেন, কারণ এগুলি জটিল স্ট্রিং অপারেশন সম্পাদনের ক্ষেত্রে তাদের বহুমুখিতা এবং দক্ষতার জন্য মূল্যবান।

## কিভাবে :

Lua প্যারল বা পাইথনের মতো ভাষাগুলির মতো একইভাবে নিয়মিত এক্সপ্রেশনকে স্বাভাবিকভাবে সমর্থন করে না। এর পরিবর্তে, এটি প্যাটার্ন ম্যাচিং সামর্থ্য প্রদান করে যা নিয়মিত এক্সপ্রেশনের অনেক সাধারণ ব্যবহার ক্ষেত্রে কাজ করে। তবে, সম্পূর্ণ নিয়মিত এক্সপ্রেশন সাপোর্টের জন্য, একজন তৃতীয়-পক্ষের লাইব্রেরি যেমন `lrexlib` ব্যবহার করতে পারেন।

### Lua-ব্যাপী প্রাথমিক প্যাটার্ন ম্যাচিং:

Lua একটি শক্তিশালী প্যাটার্ন ম্যাচিং সিস্টেম প্রদান করে যা আপনি সহজ প্রতিস্থাপন এবং অনুসন্ধানের জন্য ব্যবহার করতে পারেন:

```lua
-- সহজ অনুসন্ধান
local str = "Hello, World!"
if string.find(str, "World") then
  print("মিল পাওয়া গেছে!")
end
-- আউটপুট: মিল পাওয়া গেছে!

-- সহজ প্রতিস্থাপন
local s = string.gsub("Lua অসাধারণ!", "অসাধারণ", "দুর্দান্ত")
print(s)
-- আউটপুট: Lua দুর্দান্ত!
```

### সাবস্ট্রিং ক্যাপচার করা:

আপনি স্ট্রিং যেখানে প্যাটার্নের সাথে মিলে যায়, সেই অংশগুলি ক্যাপচার করতে পারেন:

```lua
local date = "Today is 17/05/2023."
local d, m, y = string.match(date, "(%d+)/(%d+)/(%d+)")
print("Day:", d, "Month:", m, "Year:", y)
-- আউটপুট: দিন: 17 মাস: 05 বছর: 2023
```

### `lrexlib` ব্যবহার করে নিয়মিত এক্সপ্রেশন ব্যবহার:

আসল নিয়মিত এক্সপ্রেশন ব্যবহার করতে, আপনি `lrexlib` ইনস্টল করে এবং ব্যবহার করতে পারেন। ধরুন আপনি এটি ইনস্টল করেছেন (`luarocks install lrexlib-pcre`), আপনি আরও জটিল প্যাটার্ন ম্যাচিং করতে পারেন:

```lua
local rex = require 'rex_pcre'

local text = "The rain in Spain stays mainly in the plain."
local regex = "\\bS\\w+"
local count, err = rex.gsub(text, regex, function(w)
  return w:upper()
end)
if err then
  print("এরর:", err)
else
  print("পরিবর্তিত টেক্সট:", text)
  print("প্রতিস্থাপন গণনা:", count)
end
-- উদাহরণ আউটপুট: পরিবর্তিত টেক্সট: The RAIN in SPAIN stays MAINLY in the plain.
-- প্রতিস্থাপন গণনা: 3
```

উপরের উদাহরণগুলি Lua-র নিজস্ব প্যাটার্ন ম্যাচিং সিস্টেমের মৌলিক ব্যবহার এবং `lrexlib` এর মাধ্যমে নিয়মিত এক্সপ্রেশনের শক্তি কীভাবে ব্যবহার করা যায় তা প্রদর্শন করে। আপনি যদি সাধারণ স্ট্রিং ম্যানিপুলেশন করতে চান অথবা নিয়মিত এক্সপ্রেশনের সম্পূর্ণ বহুমুখিতার দরকার পড়ে তবে, শক্তিশালী লাইব্রেরিগুলির সাথে Lua, আপনার প্রয়োজনগুলি মেটাতে পারে।
