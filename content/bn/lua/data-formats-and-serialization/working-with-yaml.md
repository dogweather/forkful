---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:22.915937-06:00
description: "YAML, \"YAML Ain't Markup Language\" \u098F\u09B0 \u09B8\u0982\u0995\
  \u09CD\u09B7\u09C7\u09AA \u09B0\u09C2\u09AA, \u098F\u099F\u09BF \u098F\u0995\u099F\
  \u09BF \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF\
  \ \u09A1\u09C7\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\
  \u0987\u099C\u09C7\u09B6\u09A8 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\
  \u09BE\u09B0\u09CD\u09A1 \u09AF\u09BE \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u09B6\
  \u0987 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\
  \u0987\u09B2 \u098F\u09AC\u0982 \u09AD\u09BE\u09B7\u09BE\u0997\u09C1\u09B2\u09CB\
  \u09B0\u2026"
lastmod: '2024-03-17T18:47:44.200686-06:00'
model: gpt-4-0125-preview
summary: "YAML, \"YAML Ain't Markup Language\" \u098F\u09B0 \u09B8\u0982\u0995\u09CD\
  \u09B7\u09C7\u09AA \u09B0\u09C2\u09AA, \u098F\u099F\u09BF \u098F\u0995\u099F\u09BF\
  \ \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF \u09A1\
  \u09C7\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\
  \u09C7\u09B6\u09A8 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\
  \u09CD\u09A1 \u09AF\u09BE \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u09B6\u0987 \u0995\
  \u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2\
  \ \u098F\u09AC\u0982 \u09AD\u09BE\u09B7\u09BE\u0997\u09C1\u09B2\u09CB\u09B0\u2026"
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

## কি এবং কেন?

YAML, "YAML Ain't Markup Language" এর সংক্ষেপ রূপ, এটি একটি মানব-পাঠযোগ্য ডেটা সিরিয়ালাইজেশন স্ট্যান্ডার্ড যা প্রায়শই কনফিগারেশন ফাইল এবং ভাষাগুলোর মধ্যে ডেটা আদান-প্রদানের জন্য ব্যবহৃত হয়। প্রোগ্রামাররা YAML এর সাধারণতা এবং পাঠযোগ্যতার কারণে এটিকে বেছে নেয়, যা এটিকে সেটিংস, বিভিন্ন এপ্লিকেশন কনফিগারেশন বা এমন কন্টেন্টের জন্য একটি পছন্দের পছন্দে পরিণত করে যা অ-প্রোগ্রামারদের দ্বারা সম্পাদনা যোগ্য হওয়া উচিত।

## যেভাবে:

Lua এ YAML এর জন্য built-in সাপোর্ট নেই, কিন্তু আপনি `lyaml` এর মতো থার্ড-পার্টি লাইব্রেরিগুলি ব্যবহার করে YAML ফাইল নিয়ে কাজ করতে পারেন। এই লাইব্রেরিটি Lua এর সাথে YAML ডেটার এনকোডিং এবং ডিকোডিং করার অনুমতি দেয়। প্রথমে, আপনাকে LuaRocks, Lua এর প্যাকেজ ম্যানেজার মাধ্যমে `lyaml` ইনস্টল করতে হবে:

```bash
luarocks install lyaml
```

### YAML ডিকোডিং:

ধরুন আপনার কাছে `config.yaml` নামক ফাইলে নিম্নলিখিত YAML কন্টেন্ট রয়েছে:

```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass
```

এই YAML ফাইলটি নিম্নলিখিত কোড দ্বারা একটি Lua টেবিলে ডিকোড করতে পারেন:

```lua
local yaml = require('lyaml')
local file = io.open("config.yaml", "r")
local content = file:read("*all")
file:close()

local data = yaml.load(content)
for k,v in pairs(data.database) do
  print(k .. ": " .. v)
end
```

যখন আপনি এই স্ক্রিপ্টটি চালাবেন, এটি আউটপুট হিসেবে দেখাবে:

```output
host: localhost
port: 3306
username: user
password: pass
```

### YAML এনকোডিং:

Lua টেবিলগুলিকে YAML ফরম্যাটে এনকোড করতে, আপনি `lyaml` দ্বারা প্রদত্ত `dump` ফাংশন ব্যবহার করবেন। ধরুন আপনি নিম্নলিখিত Lua টেবিলের একটি YAML প্রতিনিধিত্ব তৈরি করতে চান:

```lua
local data = {
  website = {
    name = "Example",
    owner = "Jane Doe",
    metadata = {
      creation_date = "2023-01-01",
      tags = {"blog", "personal", "lua"}
    }
  }
}

local yaml = require('lyaml')
local yaml_data = yaml.dump({data})
print(yaml_data)
```

আউটপুট YAML হবে:

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, personal, lua]
    name: Example
    owner: Jane Doe
```

এই ধরনের প্রক্রিয়াগুলি অনুসরণ করে, Lua প্রোগ্রামাররা বিভিন্ন অ্যাপ্লিকেশনের জন্য YAML ডেটা কার্যকরভাবে পরিচালনা করতে পারে। YAML এর সাথে এই অপারেশনগুলি অন্যান্য সিস্টেমের সাথে বা সরাসরি অন্যান্য সিস্টেমগুলির সাথে মসৃণভাবে মিথষ্ক্রিয়া করে বহুমুখী Lua অ্যাপ্লিকেশন বিকাশের জন্য এগুলি অপরিহার্য।
