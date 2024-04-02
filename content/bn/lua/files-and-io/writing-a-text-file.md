---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:40:27.849146-06:00
description: "Lua \u09A4\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\
  \u09B2\u09C7 \u09B2\u09C7\u0996\u09BE \u09AC\u09B2\u09A4\u09C7 \u098F\u0995\u099F\
  \u09BF \u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\u09C7\u0996\u09BE\u09B0 \u09AE\u09CB\
  \u09A1\u09C7 \u09A4\u09C8\u09B0\u09BF \u0985\u09A5\u09AC\u09BE \u0996\u09CB\u09B2\
  \u09BE, \u098F\u09B0\u09AA\u09B0 \u09AB\u09BE\u0987\u09B2 \u0985\u09AA\u09BE\u09B0\
  \u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AF\u09CB\u0997 \u0995\u09B0\u09BE \u09AC\
  \u09C1\u099D\u09BE\u09DF\u0964 \u098F\u099F\u09BF \u09B2\u0997\u09BF\u0982, \u09A1\
  \u09C7\u099F\u09BE \u09B8\u09CD\u099F\u09CB\u09B0\u09C7\u099C, \u0985\u09A5\u09AC\
  \u09BE\u2026"
lastmod: '2024-03-17T18:47:44.198554-06:00'
model: gpt-4-0125-preview
summary: "Lua \u09A4\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\
  \u09B2\u09C7 \u09B2\u09C7\u0996\u09BE \u09AC\u09B2\u09A4\u09C7 \u098F\u0995\u099F\
  \u09BF \u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\u09C7\u0996\u09BE\u09B0 \u09AE\u09CB\
  \u09A1\u09C7 \u09A4\u09C8\u09B0\u09BF \u0985\u09A5\u09AC\u09BE \u0996\u09CB\u09B2\
  \u09BE, \u098F\u09B0\u09AA\u09B0 \u09AB\u09BE\u0987\u09B2 \u0985\u09AA\u09BE\u09B0\
  \u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AF\u09CB\u0997 \u0995\u09B0\u09BE \u09AC\
  \u09C1\u099D\u09BE\u09DF\u0964 \u098F\u099F\u09BF \u09B2\u0997\u09BF\u0982, \u09A1\
  \u09C7\u099F\u09BE \u09B8\u09CD\u099F\u09CB\u09B0\u09C7\u099C, \u0985\u09A5\u09AC\
  \u09BE\u2026"
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কি এবং কেন?

Lua তে টেক্সট ফাইলে লেখা বলতে একটি ফাইলে লেখার মোডে তৈরি অথবা খোলা, এরপর ফাইল অপারেশন ব্যবহার করে টেক্সট যোগ করা বুঝায়। এটি লগিং, ডেটা স্টোরেজ, অথবা কনফিগারেশন ম্যানেজমেন্টের মতো কাজের জন্য একটি মৌলিক অপারেশন, যা প্রোগ্রামকে সেশনের মধ্যে ডেটা স্থিতিশীলভাবে সংরক্ষণ করতে সক্ষম করে।

## কিভাবে:

Lua তে ফাইল লেখার জন্য কাজ করা সহজ। আপনি সাধারণত `io.open()` ফাংশনটি ব্যবহার করবেন একটি ফাইল খোলার (অথবা তৈরির) জন্য, অপারেশনের মোড নির্দিষ্ট করে -- এই ক্ষেত্রে, `"w"` লেখার জন্য। যদি ফাইলটি অস্তিত্বে না থাকে, তাহলে এটি তৈরি করা হয়; যদি থাকে, তাহলে এর সামগ্রীকে প্রতিস্থাপিত করা হয়। ডেটা সঠিকভাবে সংরক্ষণ এবং সম্পদ মুক্তি পাবার জন্য লেখার পর ফাইলটি বন্ধ করা অত্যন্ত গুরুত্বপূর্ণ।

এখানে একটি সহজ উদাহরণ দেওয়া হলো যেখানে "example.txt" নামক একটি ফাইলে একটি স্ট্রিং লেখা হয়েছে:

```lua
-- ফাইলটি লেখার মোডে খোলা
local file, err = io.open("example.txt", "w")

-- ফাইল খোলার সময় ত্রুটি পরীক্ষা করা
if not file then
    print("Could not open the file: ", err)
    return
end

-- ফাইলে লেখার জন্য টেক্সট
local text = "Hello, Lua!"

-- ফাইলে টেক্সট লেখা
file:write(text)

-- ফাইল বন্ধ করা
file:close()

print("File written successfully.")
```

**নমুনা আউটপুট:**
```
File written successfully.
```

**একাধিক লাইন লেখা:**

একাধিক লাইন লেখার জন্য, আপনি আপনার টেক্সট স্ট্রিংয়ে নতুন লাইনের জন্য `\n` ব্যবহার করতে পারেন, অথবা `file:write` কে বারবার ডাকতে পারেন।

```lua
local lines = {
    "First line.",
    "Second line.",
    "Third line."
}

local file = assert(io.open("multiple_lines.txt", "w"))

for _, line in ipairs(lines) do
    file:write(line, "\n")
end

file:close()

print("Multiple lines written successfully.")
```

**নমুনা আউটপুট:**
```
Multiple lines written successfully.
```

**তৃতীয়-পক্ষের লাইব্রেরিগুলি ব্যবহার করা:**

Lua-র মানক লাইব্রেরি বেশ কার্যকর হলেও, আরও জটিল ফাইল অপারেশনের জন্য, আপনি *Penlight* এর মতো একটি তৃতীয়-পক্ষের লাইব্রেরি ব্যবহার বিবেচনা করতে পারেন। Penlight Lua-র মানক ফাইল অপারেশনগুলিকে বৃদ্ধি করে এবং ফাইল এবং ডিরেক্টরিগুলির সাথে কাজ করার জন্য সহজ উপায় প্রদান করে।

Penlight ইনস্টল করার পর, আপনি এভাবে একটি ফাইলে লিখতে পারেন:

```lua
local pl = require "pl"
local path = require "pl.path"
local file = require "pl.file"

-- লেখার জন্য টেক্সট
local text = "Hello, Penlight!"

-- Penlight ব্যবহার করে ফাইলে লেখা
local result, err = file.write("hello_penlight.txt", text)

if not result then
    print("Error writing file: ", err)
else
    print("File written successfully with Penlight.")
end
```

**নমুনা আউটপুট:**
```
File written successfully with Penlight.
```
