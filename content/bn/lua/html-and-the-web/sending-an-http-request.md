---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:18:47.853724-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Lua \u098F\u09B0 \u09AE\u09A7\u09CD\
  \u09AF\u09C7 \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 HTTP \u09B8\u09BE\u09AA\
  \u09CB\u09B0\u09CD\u099F \u09A8\u09C7\u0987, \u09A4\u09BE\u0987 \u0986\u09AE\u09B0\
  \u09BE \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BF\u0964 \u098F\u0995\u099F\u09BF \u09B8\
  \u09BE\u09A7\u09BE\u09B0\u09A3 \u09AA\u099B\u09A8\u09CD\u09A6 \u09B9\u09B2 `lua-requests`\u0964\
  \ \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\
  \u09A4 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\
  \ \u09B9\u09B2."
lastmod: '2024-03-17T18:47:44.175827-06:00'
model: gpt-4-0125-preview
summary: "Lua \u098F\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09AC\u09BF\u09B2\u09CD\
  \u099F-\u0987\u09A8 HTTP \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u09A8\u09C7\
  \u0987, \u09A4\u09BE\u0987 \u0986\u09AE\u09B0\u09BE \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BF\u0964 \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u09AA\
  \u099B\u09A8\u09CD\u09A6 \u09B9\u09B2 `lua-requests`\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u0989\u09A6\u09BE\
  \u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

## কিভাবে:
Lua এর মধ্যে বিল্ট-ইন HTTP সাপোর্ট নেই, তাই আমরা লাইব্রেরি ব্যবহার করি। একটি সাধারণ পছন্দ হল `lua-requests`। এখানে একটি দ্রুত উদাহরণ দেওয়া হল:

```lua
local requests = require('requests')

-- GET অনুরোধ
local response = requests.get('https://api.example.com/data')
print(response.status_code)
print(response.text)

-- কিছু ডেটা সহ POST অনুরোধ
local post_response = requests.post('https://api.example.com/post', {data = {key1 = 'value1', key2 = 'value2'}})
print(post_response.status_code)
print(post_response.text)
```

নমুনা আউটপুট এই রকম দেখাতে পারে:

```lua
200
"{\"data\":\"এখানে আপনার অনুরোধ করা ডেটা!\"}"

201
"{\"success\":true,\"message\":\"ডেটা গ্রহণ করা হয়েছে!\"}"
```

## গভীরভাবে দেখা
Lua-র সাধারণতা স্বাভাবিকভাবে HTTP না আবরণ করে, যেখানে লাইব্রেরিগুলি পা রাখে। `lua-requests` পাইথন রিকুয়েস্ট্স লাইব্রেরির কার্যকারিতা প্রতিফলিত করে, যা পাইথনের সাথে পরিচিত ব্যক্তিদের জন্য খুব সহজ করে তোলে।

অন্য বিকল্পগুলি অন্তর্ভুক্ত করে `LuaSocket` নিম্ন-স্তরের HTTP কাজের জন্য এবং আরও নিয়ন্ত্রণের জন্য `luasocket.http`। Lua-এ জটিল HTTP অপারেশনের জন্য `libcurl` (এর মাধ্যমে `Lua-cURL`) বাইন্ডিং রয়েছে।

ইতিহাসের দিকে তাকালে, বিল্ট-ইন HTTP সাপোর্টের অভাব প্রতিফলিত করে Lua-র এম্বেডেড-সিস্টেম মূল উদ্দেশ্য যেখানে নেটওয়ার্ক প্রোগ্রামিং কোনো অগ্রাধিকার পায়নি। বাইরের লাইব্রেরিগুলির মাধ্যমে এর উন্নতি সম্প্রদায়ের অভিযোজন ক্ষমতা এবং ভাষার প্রসারণীয়তাকে প্রতিফলিত করে।

বাস্তবায়নের দিক থেকে, যখন আপনি একটি HTTP অনুরোধ পাঠান, তা নেটওয়ার্কের মাধ্যমে নির্দিষ্ট সার্ভারে যায়। সার্ভার এটি প্রক্রিয়া করে এবং উত্তর দেয়। Lua লাইব্রেরিগুলি সকেট প্রোগ্রামিং প্রয়োজনীয়তা অ্যাবস্ট্রাক্ট করে, নেটওয়ার্ক যোগাযোগের সমস্ত জটিলতা সামলান যাতে আপনি বাস্তব অনুরোধ এবং উত্তরের উপর ফোকাস করতে পারেন।

## দেখুন এছাড়াও
- [lua-requests GitHub রিপোজিটরি](https://github.com/JakobGreen/lua-requests)
- [LuaSocket রেফারেন্স ম্যানুয়াল](http://w3.impa.br/~diego/software/luasocket/http.html)
