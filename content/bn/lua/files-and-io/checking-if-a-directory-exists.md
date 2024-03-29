---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:23.033529-06:00
description: "\u09AB\u09BE\u0987\u09B2\u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200D\u09CD\
  \u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE\u09B0 \u09B8\u09AE\u09AF\u09BC\
  \ \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09B2\u09BF\u0996\u09A4\
  \u09C7 \u0997\u09BF\u09AF\u09BC\u09C7 \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\
  \u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u09AF\u09BE\
  \u099A\u09BE\u0987 \u0995\u09B0\u09BE \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\
  \u09BF\u0995 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8, \u09AF\u09BE \u09A8\u09BF\
  \u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09C7 \u09AF\u09C7 \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u09AC\u09C8\
  \u09A7\u2026"
lastmod: '2024-03-17T18:47:44.194308-06:00'
model: gpt-4-0125-preview
summary: "\u09AB\u09BE\u0987\u09B2\u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200D\u09CD\
  \u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09BE\u09B0 \u09B8\u09AE\u09AF\u09BC\
  \ \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09B2\u09BF\u0996\u09A4\
  \u09C7 \u0997\u09BF\u09AF\u09BC\u09C7 \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\
  \u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u09AF\u09BE\
  \u099A\u09BE\u0987 \u0995\u09B0\u09BE \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\
  \u09BF\u0995 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8, \u09AF\u09BE \u09A8\u09BF\
  \u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09C7 \u09AF\u09C7 \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u09AC\u09C8\
  \u09A7\u2026"
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

ফাইলসিস্টেমের সাথে ইন্টার‍্যাক্ট করার সময় স্ক্রিপ্ট লিখতে গিয়ে ডিরেক্টরির অস্তিত্ব যাচাই করা একটি মৌলিক অপারেশন, যা নিশ্চিত করে যে আপনার প্রোগ্রাম বৈধ পাথে অপারেশন করছে এবং অবিদ্যমান ডিরেক্টরিগুলি সংক্রান্ত ত্রুটিগুলি প্রতিরোধ করা হচ্ছে। নিরাপদে ফাইল তৈরি, পড়া, অথবা ডিরেক্টরি-নির্দিষ্ট অপারেশনগুলি সঞ্চালনের জন্য এই কাজটি জরুরি।

## কিভাবে:

Lua তে, আপনার কাছে কোন বিল্ট-ইন ফাংশন নেই যেটি সরাসরি যাচাই করে যে কোন ডিরেক্টরির অস্তিত্ব আছে কিনা, তাই আপনি প্রায়শই Lua File System (lfs) লাইব্রেরীটির উপর নির্ভর করেন, যা একটি জনপ্রিয় থার্ড-পার্টি লাইব্রেরী ফাইল অপারেশনের জন্য।

প্রথমে, নিশ্চিত করুন যে আপনার কাছে Lua File System ইন্সটল করা আছে। যদি না থাকে, আপনি সাধারণত LuaRocks ব্যবহার করে এটি ইন্সটল করতে পারেন:

```sh
luarocks install luafilesystem
```

এরপর, ডিরেক্টরির অস্তিত্ব যাচাই করার জন্য নিম্নলিখিত উদাহরণটি ব্যবহার করতে পারেন:

```lua
local lfs = require "lfs"

function directoryExists(directory)
    local attr = lfs.attributes(directory)
    return attr and attr.mode == "directory"
end

-- একটি নির্দিষ্ট ডিরেক্টরির অস্তিত্ব যাচাই করুন
if directoryExists("/path/to/your/directory") then
    print("Directory exists.")
else
    print("Directory does not exist.")
end
```

এটি আউটপুট করবে:

```
Directory exists.
```

অথবা, যদি ডিরেক্টরিটি অস্তিত্ব না থাকে:

```
Directory does not exist.
```

এই পদ্ধতিটি `lfs.attributes` ফাংশনটি ব্যবহার করে পাথের অ্যাট্রিবিউটগুলি পেতে, যদি পাথটি অস্তিত্ব আছে এবং এর `mode` অ্যাট্রিবিউটটি `directory` হয়, তবে ডিরেক্টরির অস্তিত্ব নিশ্চিত করা হয়।
