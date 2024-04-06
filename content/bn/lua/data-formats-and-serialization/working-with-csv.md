---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:47.260968-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Lua \u098F, CSV \u09AB\u09BE\u0987\
  \u09B2\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\
  \ \u09AD\u09BE\u09B7\u09BE\u099F\u09BF \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u09AA\
  \u09CD\u09B0\u09A6\u09A4\u09CD\u09A4 \u09AE\u09CC\u09B2\u09BF\u0995 \u09AB\u09BE\
  \u0987\u09B2 IO \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0995\u09B0\u09BE \u09AF\u09C7\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7, \u09B8\u09B9\u099C \u0995\u09BE\u099C\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.202899-06:00'
model: gpt-4-0125-preview
summary: "Lua \u098F, CSV \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AD\u09BE\u09B7\u09BE\u099F\u09BF \u09A6\
  \u09CD\u09AC\u09BE\u09B0\u09BE \u09AA\u09CD\u09B0\u09A6\u09A4\u09CD\u09A4 \u09AE\
  \u09CC\u09B2\u09BF\u0995 \u09AB\u09BE\u0987\u09B2 IO \u0985\u09AA\u09BE\u09B0\u09C7\
  \u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0995\
  \u09B0\u09BE \u09AF\u09C7\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7, \u09B8\u09B9\u099C\
  \ \u0995\u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09BE\u09B9\u09CD\
  \u09AF\u09BF\u0995 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\
  \u09B2\u09BF\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u099B\u09BE\
  \u09A1\u09BC\u09BE\u0987\u0964 \u0986\u09B0\u0993 \u099C\u099F\u09BF\u09B2 \u0985\
  \u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF, \u09AF\
  \u09C7\u09AE\u09A8 \u09AC\u09BF\u09B6\u09C7\u09B7 \u0995\u09CD\u09B7\u09C7\u09A4\
  \u09CD\u09B0\u0997\u09C1\u09B2\u09BF \u09B8\u09BE\u09AE\u09B2\u09BE\u09A8\u09CB\
  \ (\u0989\u09A6\u09BE\u09B9\u09B0\u09A3\u09B8\u09CD\u09AC\u09B0\u09C2\u09AA, \u09AE\
  \u09BE\u09A8\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u0995\u09AE\u09BE), `lua-csv`\
  \ \u09AE\u09A4\u09CB \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\u09B7\
  \u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\
  \u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE \u0989\u09AA\
  \u0995\u09BE\u09B0\u09C0 \u09B9\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964\n\n\u098F\
  \u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2 \u09AF\
  \u09C7\u09AD\u09BE\u09AC\u09C7 \u09B2\u09BE\u0987\u09A8 \u0985\u09A8\u09C1\u09AF\
  \u09BE\u09AF\u09BC\u09C0 \u098F\u0995\u099F\u09BF CSV \u09AB\u09BE\u0987\u09B2 \u09AA\
  \u09A1\u09BC\u09A4\u09C7 \u09B9\u09AF\u09BC, \u09AA\u09CD\u09B0\u09A4\u09BF\u099F\
  \u09BF \u09B2\u09BE\u0987\u09A8\u0995\u09C7 \u0995\u09AE\u09BE \u09AC\u09BF\u09AD\
  \u09BE\u099C\u0995\u09C7\u09B0 \u09AD\u09BF\u09A4\u09CD\u09A4\u09BF\u09A4\u09C7\
  \ \u09AE\u09BE\u09A8\u09C7 \u09AC\u09BF\u09AD\u0995\u09CD\u09A4 \u0995\u09B0\u09C7\
  \u0964."
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কিভাবে:
Lua এ, CSV ফাইলের সাথে কাজ করা ভাষাটি দ্বারা প্রদত্ত মৌলিক ফাইল IO অপারেশন ব্যবহার করে করা যেতে পারে, সহজ কাজের জন্য বাহ্যিক লাইব্রেরিগুলির প্রয়োজন ছাড়াই। আরও জটিল অপারেশনের জন্য, যেমন বিশেষ ক্ষেত্রগুলি সামলানো (উদাহরণস্বরূপ, মানের মধ্যে কমা), `lua-csv` মতো তৃতীয় পক্ষের লাইব্রেরিগুলি ব্যবহার করা উপকারী হতে পারে।

### একটি CSV ফাইল পড়া
এখানে একটি সহজ উদাহরণ দেওয়া হল যেভাবে লাইন অনুযায়ী একটি CSV ফাইল পড়তে হয়, প্রতিটি লাইনকে কমা বিভাজকের ভিত্তিতে মানে বিভক্ত করে।

```lua
function parseCSVLine(line)
    local result = {}
    local from = 1
    local sep = ","
    local field
    while true do
        local start, finish = string.find(line, sep, from)
        if not start then
            table.insert(result, string.sub(line, from))
            break
        end
        field = string.sub(line, from, start - 1)
        table.insert(result, field)
        from = finish + 1
    end
    return result
end

local file = io.open("example.csv", "r")
for line in file:lines() do
    local values = parseCSVLine(line)
    for i, v in ipairs(values) do
        print(i, v)
    end
end
file:close()
```

**নমুনা আউটপুট** (সামগ্রী "name,age\newlineJohn Doe,30\newlineJane Doe,32" সহ একটি `example.csv`-এর জন্য):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### একটি CSV ফাইল লেখা
একটি CSV ফাইল তৈরি করতে, আপনি কেবল কমা-বিভক্ত মানের স্ট্রিংগুলি নির্মাণ করে এবং সেগুলি লাইন অনুযায়ী ফাইলে লেখেন।

```lua
local data = {
    {"name", "age"},
    {"John Doe", "30"},
    {"Jane Doe", "32"}
}

local file = io.open("output.csv", "w")
for _, v in ipairs(data) do
    file:write(table.concat(v, ","), "\n")
end
file:close()
```

এটি উল্লেখিত ডেটা সহ একটি `output.csv` ফাইল তৈরি করবে (অথবা লিখন প্রতিস্থাপন করবে)।

### lua-csv ব্যবহার করা
উদ্ধৃতি এবং এস্কেপ অক্ষরের সমর্থন সহ উন্নত CSV হ্যান্ডলিংয়ের জন্য, `lua-csv` লাইব্রেরিটি একটি শক্তিশালী পছন্দ হতে পারে।

প্রথমে, এটি LuaRocks ব্যবহার করে ইনস্টল করুন:
```shell
luarocks install lua-csv
```

তারপর, একটি CSV ফাইল পড়া যেতে পারে এতো সহজে:

```lua
local csv = require("csv")

-- ফাইল থেকে পড়া
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

এবং উপযুক্ত উদ্ধৃতি এবং এস্কেপিং সহ একটি CSV-তে লেখা:

```lua
local file = csv.open("output.csv", {write=true})

local data = {
    {"name", "profession", "location"},
    {"John Doe", "Software Engineer", "New York, NY"},
    {"Jane Doe", "Data Scientist", "\"San Francisco, CA\""}
}

for _, v in ipairs(data) do
    file:write(v)
end
```

এই পদ্ধতি মানের মধ্যে বিরাজমান কমা এবং উদ্ধৃতিগুলির মত জটিলতাগুলি স্বয়ংক্রিয়ভাবে হ্যান্ডল করে।
