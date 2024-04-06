---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:46.898335-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B2\u09C1\u09AF\u09BC\u09BE\
  , \u09B9\u09BE\u09B2\u0995\u09BE \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u09B6\u0995\
  \u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u098F\u0995\u099F\u09BF \u09B8\u09CD\
  \u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09BF\u0982 \u09AD\u09BE\u09B7\u09BE\
  \ \u09B9\u09DF\u09C7\u0993, \u0995\u09CB\u09A8\u09CB \u09AC\u09BF\u09B2\u09CD\u099F\
  -\u0987\u09A8 \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982 \u09AB\u09CD\u09B0\u09C7\
  \u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995 \u09B8\u09AE\u09CD\u09AC\u09B2\u09BF\
  \u09A4 \u09A8\u09BE\u0964 \u09A4\u09AC\u09C7, \u09AC\u09BE\u09B8\u09CD\u099F\u09C7\
  \u09A1 \u098F\u09AC\u0982 \u09B2\u09C1\u09DF\u09BE \u0987\u0989\u09A8\u09BF\u099F\
  \ \u098F\u09B0 \u09AE\u09A4\u09CB\u2026"
lastmod: '2024-04-05T21:53:52.628902-06:00'
model: gpt-4-0125-preview
summary: "\u09B2\u09C1\u09AF\u09BC\u09BE, \u09B9\u09BE\u09B2\u0995\u09BE \u0995\u09BF\
  \u09A8\u09CD\u09A4\u09C1 \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\
  \u09BF\u0982 \u09AD\u09BE\u09B7\u09BE \u09B9\u09DF\u09C7\u0993, \u0995\u09CB\u09A8\
  \u09CB \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u099F\u09C7\u09B8\u09CD\u099F\
  \u09BF\u0982 \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995\
  \ \u09B8\u09AE\u09CD\u09AC\u09B2\u09BF\u09A4 \u09A8\u09BE\u0964 \u09A4\u09AC\u09C7\
  , \u09AC\u09BE\u09B8\u09CD\u099F\u09C7\u09A1 \u098F\u09AC\u0982 \u09B2\u09C1\u09DF\
  \u09BE \u0987\u0989\u09A8\u09BF\u099F \u098F\u09B0 \u09AE\u09A4\u09CB \u09A5\u09BE\
  \u09B0\u09CD\u09A1-\u09AA\u09BE\u09B0\u09CD\u099F\u09BF \u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF \u099F\u09C7\u09B8\u09CD\u099F\
  \u09BF\u0982 \u0985\u09AA\u09C7\u0995\u09CD\u09B7\u09BE\u0995\u09C3\u09A4 \u09B8\
  \u09B9\u099C \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7, \u0986\u09AE\u09B0\u09BE \u0989\u09AD\u09AF\u09BC\u09C7\u09B0 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\
  \ \u09A6\u09C7\u0996\u09AC\u0964."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কিভাবে:
লুয়া, হালকা কিন্তু শক্তিশালী একটি স্ক্রিপ্টিং ভাষা হয়েও, কোনো বিল্ট-ইন টেস্টিং ফ্রেমওয়ার্ক সম্বলিত না। তবে, বাস্টেড এবং লুয়া ইউনিট এর মতো থার্ড-পার্টি লাইব্রেরিগুলি টেস্টিং অপেক্ষাকৃত সহজ করে তোলে। এখানে, আমরা উভয়ের ব্যবহারের উদাহরণ দেখব।

### বাস্টেড ব্যবহার করে
বাস্টেড হলো একটি জনপ্রিয় লুয়া টেস্টিং ফ্রেমওয়ার্ক যা টেস্ট লেখার একটি নমনীয় উপায় প্রদান করে। প্রথমত, লুয়ারকস মাধ্যমে বাস্টেড ইনস্টল করুন (`luarocks install busted`). ইনস্টল করার পর, আপনি আপনার টেস্টগুলি লিখতে পারেন। এখানে দুইটি সংখ্যা যোগ করা `add` ফাংশনের জন্য একটি সিম্পল টেস্ট রয়েছে:

```lua
-- add.lua
local function add(a, b)
  return a + b
end

return add
```

```lua
-- add_spec.lua
local add = require('add')

describe("Add function", function()
  it("সঠিকভাবে দুই সংখ্যা যোগ করা উচিত", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

টেস্টগুলি চালানোর জন্য, আপনার টার্মিনালে `busted` কমান্ড চালান। পাস হওয়া একটি টেস্টের জন্য নমুনা আউটপুট দেখতে পারেন এরকম:

```
●
1 সফলতা / 0 ব্যর্থতা / 0 ত্রুটি / 0 মুলতুবি : 0.002 সেকেন্ড
```

### লুয়া ইউনিট ব্যবহার করে
লুয়া ইউনিট হলো আরেকটি টেস্টিং ফ্রেমওয়ার্ক যেটি xUnit কনভেনশনগুলি অনুসরণ করে এবং সেটআপ করা সহজ। লুয়ারকস ব্যবহার করে `luarocks install luaunit` দিয়ে লুয়া ইউনিট ইনস্টল করুন। উপরের মতো একটি সিমিলার টেস্ট আপনি লুয়া ইউনিট দিয়ে এভাবে লিখতে পারেন:

```lua
-- add.lua একই থাকে

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

এই স্ক্রিপ্টটি সরাসরি লুয়া দিয়ে চালালে (`lua test_add.lua`) আউটপুট আসবে এরকম:

```
.
Ran 1 tests in 0.001 seconds, 1 success, 0 failures
```

বাস্টেড এবং লুয়া ইউনিট উভয়ই বিভিন্ন টেস্টিং পরিস্থিতি সামলানোর জন্য বিস্তৃত বৈশিষ্ট্য অফার করে, যাতে মকিং, স্পাইং এবং অ্যাসিঙ্ক্রোনাস টেস্টিং অন্তর্ভুক্ত। এদের মধ্যে নির্বাচন করা আপনার প্রকল্পের বিশেষ চাহিদা এবং সিনট্যাক্স এবং ফাংশনালিটি সম্পর্কে আপনার ব্যক্তিগত পছন্দের উপর নির্ভর করবে।
