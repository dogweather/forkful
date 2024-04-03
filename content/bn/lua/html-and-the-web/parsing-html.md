---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:04:56.597638-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Lua-\u098F HTML \u09AA\u09BE\u09B0\
  \u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\u09B2\u09CD\
  \u099F-\u0987\u09A8 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09A8\
  \u09C7\u0987, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u0986\u09AA\u09A8\u09BF `LuaHTML`\
  \ \u0985\u09A5\u09AC\u09BE `LuaXML` \u098F\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\
  \u09C7 `libxml2` \u098F\u09B0 \u09AC\u09BE\u0987\u09A8\u09CD\u09A1\u09BF\u0982\u09B8\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A4\u09C3\u09A4\
  \u09C0\u09AF\u09BC \u09AA\u0995\u09CD\u09B7\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.176804-06:00'
model: gpt-4-0125-preview
summary: "Lua-\u098F HTML \u09AA\u09BE\u09B0\u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09A8\u09C7\u0987, \u0995\u09BF\u09A8\u09CD\
  \u09A4\u09C1 \u0986\u09AA\u09A8\u09BF `LuaHTML` \u0985\u09A5\u09AC\u09BE `LuaXML`\
  \ \u098F\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 `libxml2` \u098F\u09B0\
  \ \u09AC\u09BE\u0987\u09A8\u09CD\u09A1\u09BF\u0982\u09B8 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\
  \u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0995\u099F\u09BF \u099C\
  \u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\
  \u09AF\u09BC\u09BE \u09B9\u09B2 `lua-gumbo` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\
  , \u09AF\u09BE \u09B8\u09CB\u099C\u09BE, HTML5-\u0985\u09A8\u09C1\u09B8\u09B0\u09A3\
  \u09C0\u09AF\u09BC \u09AA\u09BE\u09B0\u09B8\u09BF\u0982 \u0995\u09CD\u09B7\u09AE\
  \u09A4\u09BE \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964\n\n#."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কিভাবে:
Lua-এ HTML পারস করার জন্য বিল্ট-ইন লাইব্রেরি নেই, কিন্তু আপনি `LuaHTML` অথবা `LuaXML` এর মাধ্যমে `libxml2` এর বাইন্ডিংস ব্যবহার করে তৃতীয় পক্ষের লাইব্রেরিগুলি ব্যবহার করতে পারেন। একটি জনপ্রিয় প্রক্রিয়া হল `lua-gumbo` লাইব্রেরি ব্যবহার করা, যা সোজা, HTML5-অনুসরণীয় পারসিং ক্ষমতা প্রদান করে।

### lua-gumbo ইন্সটল করা:
প্রথমে, নিশ্চিত করুন `lua-gumbo` ইন্সটল করা আছে। সাধারণত আপনি এটি luarocks ব্যবহার করে ইন্সটল করতে পারেন:

```sh
luarocks install lua-gumbo
```

### lua-gumbo দিয়ে মৌলিক পারসিং:
এখানে আপনি কিভাবে একটি সাধারণ HTML স্নিপেট পারস করতে এবং তার থেকে ডেটা সংগ্রহ করতে পারেন `lua-gumbo` ব্যবহার করে:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse[[<html><body><p>Hello, world!</p></body></html>]]

local p = document:getElementsByTagName("p")[1]
print(p.textContent)  -- Output: Hello, world!
```

### উন্নত উদাহরণ - লিঙ্ক এক্সট্র্যাক্ট করা:
একটি HTML ডকুমেন্টে সমস্ত অ্যাংকর ট্যাগ (`<a>` উপাদান) থেকে `href` অ্যাট্রিবিউট এক্সট্র্যাক্ট করার জন্য:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse([[
<html>
<head><title>Sample Page</title></head>
<body>
  <a href="http://example.com/1">Link 1</a>
  <a href="http://example.com/2">Link 2</a>
  <a href="http://example.com/3">Link 3</a>
</body>
</html>
]])

for _, element in ipairs(document.links) do
    if element.getAttribute then  -- Ensure it's an Element and has attributes
        local href = element:getAttribute("href")
        if href then print(href) end
    end
end

-- নমুনা আউটপুট:
-- http://example.com/1
-- http://example.com/2
-- http://example.com/3
```

এই কোড স্নিপেটটি ডকুমেন্টের সমস্ত লিংকগুলি ঘুরে তাদের `href` অ্যাট্রিবিউট প্রিন্ট করে। `lua-gumbo` লাইব্রেরির HTML ডকুমেন্টের গঠন বোঝা এবং পারস করার ক্ষমতা নির্দিষ্ট ট্যাগ বা অ্যাট্রিবিউট অনুসারে নির্দিষ্ট উপাদানগুলি এক্সট্র্যাক্ট করার প্রক্রিয়াটিকে সহজ করে তোলে।
