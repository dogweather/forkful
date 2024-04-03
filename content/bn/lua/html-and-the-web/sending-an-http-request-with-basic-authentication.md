---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:19:38.083651-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Lua-\u098F\u09B0 \u09AE\u09A7\u09CD\
  \u09AF\u09C7 HTTP \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u0985\u09A8\u09CD\u09A4\
  \u09B0\u09CD\u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09A8\u09C7\u0987, \u09A4\
  \u09BE\u0987 \u0986\u09AA\u09A8\u09BE\u09B0 `socket.http` \u09A5\u09C7\u0995\u09C7\
  \ LuaSocket \u09AC\u09BE Lua 5.3+ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09B2\u09C7 `http` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \ \u09A5\u09C7\u0995\u09C7 `http.request`\u2026"
lastmod: '2024-03-17T18:47:44.178814-06:00'
model: gpt-4-0125-preview
summary: "Lua-\u098F\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 HTTP \u09B8\u09BE\u09AA\u09CB\
  \u09B0\u09CD\u099F \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09A8\u09BF\u09B0\u09CD\u09AE\
  \u09BF\u09A4 \u09A8\u09C7\u0987, \u09A4\u09BE\u0987 \u0986\u09AA\u09A8\u09BE\u09B0\
  \ `socket.http` \u09A5\u09C7\u0995\u09C7 LuaSocket \u09AC\u09BE Lua 5.3+ \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09B2\u09C7 `http` \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09A5\u09C7\u0995\u09C7 `http.request` \u098F\
  \u09B0 \u09AE\u09A4\u09CB \u098F\u0995\u099F\u09BF \u09AC\u09BE\u09B9\u09CD\u09AF\
  \u09BF\u0995 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AA\u09CD\
  \u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09B9\u09AC\u09C7\u0964 \u09AE\u09CC\u09B2\
  \u09BF\u0995 \u09AA\u09CD\u09B0\u09AE\u09BE\u09A3\u09C0\u0995\u09B0\u09A3\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF, \u0995\u09CD\u09B0\u09C7\u09A1\u09C7\u09A8\u09B6\u09BF\
  \u09AF\u09BC\u09BE\u09B2 \u098F\u09A8\u0995\u09CB\u09A1 \u0995\u09B0\u09C1\u09A8\
  \ \u098F\u09AC\u0982 \u098F\u099F\u09BF \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09B9\
  \u09C7\u09A1\u09BE\u09B0\u09C7 \u09AF\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09C1\
  \u09A8\u0964."
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
weight: 45
---

## কিভাবে:
Lua-এর মধ্যে HTTP সাপোর্ট অন্তর্নির্মিত নেই, তাই আপনার `socket.http` থেকে LuaSocket বা Lua 5.3+ ব্যবহার করলে `http` লাইব্রেরি থেকে `http.request` এর মতো একটি বাহ্যিক লাইব্রেরি প্রয়োজন হবে। মৌলিক প্রমাণীকরণের জন্য, ক্রেডেনশিয়াল এনকোড করুন এবং এটি অনুরোধ হেডারে যুক্ত করুন।

```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")
local mime = require("mime")

-- আপনার ক্রেডেনশিয়াল
local username = "Aladdin"
local password = "openSesame"
local credentials = mime.b64(username .. ":" .. password)

-- অনুরোধ সেটআপ
local response_body = {}
local res, code, response_headers = http.request{
    url = "http://example.com/data",
    method = "GET",
    headers = {
        ["Authorization"] = "Basic " .. credentials
    },
    sink = ltn12.sink.table(response_body)
}

-- ফলাফল আউটপুট
if code == 200 then
    print(table.concat(response_body))
else
    print("ত্রুটি: " .. (res or code))
end
```

## গভীর ডাইভ
HTTP মৌলিক প্রমাণীকরণ হচ্ছে একটি পদ্ধতি যেখানে একটি HTTP ইউজার এজেন্ট একটি অনুরোধ করার সময় একটি ইউজার নেম এবং পাসওয়ার্ড প্রদান করে। ওয়েবের ইতিহাসের প্রাথমিক সময়ে আবিষ্কার করা হলেও, এটি ব্যাপকভাবে

সমর্থিত হলেও খুব নিরাপদ নয়; ক্রেডেনশিয়ালগুলি কেবল বেস64-এনকোড করা হয়, এনক্রিপ্ট করা নয়।

বিকল্পগুলি অন্তর্ভুক্ত ডাইজেস্ট প্রমাণীকরণ, OAuth, এবং API কী – সবই আরও শক্তিশালী নিরাপত্তা প্রদান করে। মৌলিক প্রমাণীকরণ প্রায়শই স্ক্রিপ্টিং দ্রুত পরীক্ষা, ইন্টারনাল টুলস, বা HTTPS এর মাধ্যমে পরিবহন নিরাপদ হলে ব্যবহৃত হয়।

Lua-তে মৌলিক প্রমাণীকরণ বাস্তবায়ন করতে, আপনি সাধারণত একটি স্ট্রিং তৈরি করেন যা ইউজারনেম এবং পাসওয়ার্ডকে একটি কোলন দ্বারা পৃথক করে, তারপর সেই স্ট্রিংটিকে base64 এর মাধ্যমে এনকোড করেন। এই এনকোডেড স্ট্রিংটি আপনার HTTP অনুরোধের `Authorization` হেডারে পাঠানো হয়।

Lua-এর নমনীয় প্রকৃতি মানে হল HTTP এবং base64 এনকোডিং হ্যান্ডেল করার জন্য আপনি লাইব্রেরির ওপর বিকল্প পছন্দ করতে পারেন। LuaSocket দীর্ঘকাল ধরে নেটওয়ার্ক অপারেশনের জন্য যাও ব্যবহৃত হচ্ছে, যদিও Lua-এর নতুন ভার্সন যেমন অধিক জটিল কাজের জন্য `http` লাইব্রেরি বা `CURL` বাইন্ডিংস মতো বিকল্প প্রবর্তন করেছে।

## দেখুন
- LuaSocket ডকুমেন্টেশন: http://w3.impa.br/~diego/software/luasocket/http.html
- HTTPS সাপোর্টের জন্য LuaSec: https://github.com/brunoos/luasec/wiki
- HTTP প্রমাণীকরণের একটি প্রবেশিকা: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- RFC 2617 – HTTP প্রমাণীকরণ: মৌলিক এবং ডাইজেস্ট অ্যাক্সেস প্রমাণীকরণ: https://tools.ietf.org/html/rfc2617
