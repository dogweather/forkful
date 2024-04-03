---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:50.726352-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B2\u09C1\u09DF\u09BE\u09DF\
  , `string.sub` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C1\u09A8."
lastmod: '2024-03-17T18:47:44.166500-06:00'
model: gpt-4-0125-preview
summary: "\u09B2\u09C1\u09DF\u09BE\u09DF, `string.sub` \u09AB\u09BE\u0982\u09B6\u09A8\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8."
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কিভাবে:
লুয়ায়, `string.sub` ফাংশন ব্যবহার করুন:

```lua
local text = "Hello, Lua!"
-- 'Hello' বের করে আনুন
print(string.sub(text, 1, 5)) -- আউটপুট: Hello

-- 'Lua' ধরুন
print(string.sub(text, 8, 11)) -- আউটপুট: Lua
```

অথবা নেগেটিভ ইনডেক্সের সাহায্যে শেষের অক্ষরগুলো নিন:

```lua
-- শেষ থেকে 'Lua!' বের করে আনুন
print(string.sub(text, -4)) -- আউটপুট: Lua!
```

প্যাটার্ন ব্যবহার করে খুঁজে বের করে আনুন:

```lua
local phrase = "The quick brown fox jumps"
-- ম্যাচ করে 'quick' বের করে আনুন
print(phrase:match("(%a+) quick")) -- আউটপুট: The
```

## গভীর ডুব
প্রারম্ভিক প্রোগ্রামিং যুগে, স্ট্রিং হ্যান্ডলিং ম্যানুয়াল ও অসুবিধাজনক ছিল, প্রায়ই লুপ এবং কন্ডিশনালের প্রয়োজন হতো। লুয়ার `string.sub` তার সমৃদ্ধ স্ট্রিং লাইব্রেরির একটি অংশ, যা স্ট্রিং ম্যানিপুলেশনকে সহজ করে দেয়। `string.sub`-এর বিকল্প হিসাবে `string.match` দ্বারা প্যাটার্ন ম্যাচিং আছে, যা আরও শক্তিশালী কিন্তু সাধারণ কাজের জন্য অতিরিক্ত হতে পারে।

`string.sub` এবং প্যাটার্ন ম্যাচিং লুয়ার সি রুটের কারণে সি ফাংশনগুলির উপর ভিত্তি করে আছে। লুয়ায় স্ট্রিংগুলির জন্য পাইথনের মতো বিস্তৃত স্ট্যান্ডার্ড লাইব্রেরি খুঁজে পাবেন না; এটি মূলতঃ সাধারণতা ও দক্ষতাকে গুরুত্ব দেয়। মনে রাখবেন, লুয়ায় ইনডেক্স বরাবর 1 থেকে শুরু হয়, 0 থেকে নয়।

## আরও দেখুন
- স্ট্রিংগুলির উপর লুয়া 5.4 রেফারেন্স ম্যানুয়াল: [www.lua.org/manual/5.4/manual.html#6.4](https://www.lua.org/manual/5.4/manual.html#6.4)
- 'লুয়ায় প্রোগ্রামিং' (৪র্থ সংস্করণ), বিশেষত স্ট্রিংগুলির উপর অধ্যায়: [www.lua.org/pil/contents.html](https://www.lua.org/pil/contents.html)
