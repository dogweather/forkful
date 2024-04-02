---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:36.203365-06:00
description: "Lua-\u09A4\u09C7 JSON \u098F\u09B0 \u09B8\u0999\u09CD\u0997\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 JSON-\u09AB\u09B0\u09CD\
  \u09AE\u09CD\u09AF\u09BE\u099F\u09C7\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \u0997\u09C1\u09B2\u09BF\u0995\u09C7 Lua \u099F\u09C7\u09AC\u09BF\u09B2\u09C7 \u09AA\
  \u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09AC\u09BF\u09AA\
  \u09B0\u09C0\u09A4\u09C7, \u09AF\u09BE Lua \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\
  \u09BF\u0995\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09CB \u098F\u09AC\u0982 \u0993\
  \u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\u09B8 \u09AC\u09BE\
  \ \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995\u2026"
lastmod: '2024-03-17T18:47:44.201762-06:00'
model: gpt-4-0125-preview
summary: "Lua-\u09A4\u09C7 JSON \u098F\u09B0 \u09B8\u0999\u09CD\u0997\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 JSON-\u09AB\u09B0\u09CD\
  \u09AE\u09CD\u09AF\u09BE\u099F\u09C7\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \u0997\u09C1\u09B2\u09BF\u0995\u09C7 Lua \u099F\u09C7\u09AC\u09BF\u09B2\u09C7 \u09AA\
  \u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09AC\u09BF\u09AA\
  \u09B0\u09C0\u09A4\u09C7, \u09AF\u09BE Lua \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\
  \u09BF\u0995\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09CB \u098F\u09AC\u0982 \u0993\
  \u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\u09B8 \u09AC\u09BE\
  \ \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995\u2026"
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কি ও কেন?
Lua-তে JSON এর সঙ্গে কাজ করা মানে JSON-ফর্ম্যাটের স্ট্রিংগুলিকে Lua টেবিলে পার্স করা এবং বিপরীতে, যা Lua অ্যাপ্লিকেশনগুলো এবং ওয়েব সার্ভিস বা বাহ্যিক API-এর মধ্যে সহজে ডেটা বিনিময়ের সুযোগ সৃষ্টি করে। প্রোগ্রামাররা এটি করে থাকেন JSON-এর হাল্কা ও সহজে-পার্সযোগ্য ফর্ম্যাটের সুবিধা নিতে, যা তথ্য সঞ্চয়, কনফিগারেশন, অথবা API যোগাযোগকে কার্যকর করে তোলে।

## কিভাবে:
Lua-এ JSON প্রক্রিয়াজাতকরণের জন্য নির্মিত পাঠাগার অন্তর্ভুক্ত নেই। তাই, একটি জনপ্রিয় তৃতীয়-পক্ষের পাঠাগার হল `dkjson`, যা আপনি JSON এনকোডিং এবং ডিকোডিং এর জন্য সহজেই ব্যবহার করতে পারেন। প্রথমে, `dkjson` ইনস্টল নিশ্চিত করুন, উদাহরণস্বরূপ, LuaRocks এর মাধ্যমে (`luarocks install dkjson`), এবং এরপর নিচের উদাহরণগুলি অনুসরণ করুন।

### JSON কে Lua টেবিলে ডিকোডিং
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Lua Programmer", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("Error:", err)
else
  print("Name:", luaTable.name) -- আউটপুট: Name: Lua Programmer
  print("Age:", luaTable.age) -- আউটপুট: Age: 30
  print("Languages:", table.concat(luaTable.languages, ", ")) -- আউটপুট: Languages: Lua, JavaScript
end
```

### Lua টেবিল থেকে JSON-এ এনকোডিং
```lua
local dkjson = require "dkjson"

local luaTable = {
  name = "Lua Programmer",
  age = 30,
  languages = { "Lua", "JavaScript" }
}

local jsonString = dkjson.encode(luaTable, { indent = true })
print(jsonString)
```

এনকোডিং এর জন্য নমুনা আউটপুট:
```json
{
  "age": 30,
  "languages": [
    "Lua",
    "JavaScript"
  ],
  "name": "Lua Programmer"
}
```

এই সাধারণ উদাহরণগুলি Lua-তে JSON এর সঙ্গে কাজ করার উপায় প্রদর্শন করে, যা Lua অ্যাপ্লিকেশনগুলিকে বিভিন্ন ওয়েব প্রযুক্তি এবং বাহ্যিক API-এর সাথে সহজে একীভূত করে। মনে রাখবেন, `dkjson` এই উদাহরণগুলিতে ব্যবহৃত হলেও, আপনার প্রকল্পের চাহিদা অনুযায়ী `cjson` এবং `RapidJSON` এর মতো অন্যান্য পাঠাগারগুলিও উপযুক্ত বিকল্প হতে পারে।
