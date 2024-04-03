---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:36.203365-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Lua-\u098F JSON \u09AA\u09CD\u09B0\
  \u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u099C\u09BE\u09A4\u0995\u09B0\u09A3\u09C7\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09AA\
  \u09BE\u09A0\u09BE\u0997\u09BE\u09B0 \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\
  \u0995\u09CD\u09A4 \u09A8\u09C7\u0987\u0964 \u09A4\u09BE\u0987, \u098F\u0995\u099F\
  \u09BF \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09A4\u09C3\u09A4\u09C0\
  \u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09AA\u09BE\u09A0\u09BE\u0997\
  \u09BE\u09B0 \u09B9\u09B2 `dkjson`, \u09AF\u09BE \u0986\u09AA\u09A8\u09BF JSON \u098F\
  \u09A8\u0995\u09CB\u09A1\u09BF\u0982 \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.201762-06:00'
model: gpt-4-0125-preview
summary: "Lua-\u098F JSON \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\
  \u099C\u09BE\u09A4\u0995\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09A8\
  \u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09AA\u09BE\u09A0\u09BE\u0997\u09BE\u09B0\
  \ \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u09A8\u09C7\
  \u0987\u0964 \u09A4\u09BE\u0987, \u098F\u0995\u099F\u09BF \u099C\u09A8\u09AA\u09CD\
  \u09B0\u09BF\u09AF\u09BC \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\
  \u09B7\u09C7\u09B0 \u09AA\u09BE\u09A0\u09BE\u0997\u09BE\u09B0 \u09B9\u09B2 `dkjson`,\
  \ \u09AF\u09BE \u0986\u09AA\u09A8\u09BF JSON \u098F\u09A8\u0995\u09CB\u09A1\u09BF\
  \u0982 \u098F\u09AC\u0982 \u09A1\u09BF\u0995\u09CB\u09A1\u09BF\u0982 \u098F\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09B8\u09B9\u099C\u09C7\u0987 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964\
  \ \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, `dkjson` \u0987\u09A8\u09B8\u09CD\u099F\u09B2\
  \ \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09C1\u09A8, \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3\u09B8\u09CD\u09AC\u09B0\u09C2\u09AA, LuaRocks \u098F\u09B0\
  \ \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 (`luarocks install dkjson`), \u098F\
  \u09AC\u0982 \u098F\u09B0\u09AA\u09B0 \u09A8\u09BF\u099A\u09C7\u09B0 \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3\u0997\u09C1\u09B2\u09BF \u0985\u09A8\u09C1\u09B8\u09B0\u09A3\
  \ \u0995\u09B0\u09C1\u09A8\u0964\n\n#."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

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
