---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:43:04.147127-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\
  \u09A1 \u098F\u09B0\u09B0 (stderr) \u09A4\u09C7 \u09B2\u09C7\u0996\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u09B9\u09B2 \u098F\u09B0\u09B0 \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE\
  \ \u098F\u09AC\u0982 \u09A1\u09BE\u09DF\u09BE\u0997\u09A8\u09B8\u09CD\u099F\u09BF\
  \u0995 \u0986\u0989\u099F\u09AA\u09C1\u099F\u0997\u09C1\u09B2\u09BF \u098F\u0995\
  \u099F\u09BF \u0986\u09B2\u09BE\u09A6\u09BE \u099A\u09CD\u09AF\u09BE\u09A8\u09C7\
  \u09B2\u09C7 \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09BF\u09A4 \u0995\u09B0\u09BE\
  , \u09AF\u09BE \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\
  \u09CD\u09A1 \u0986\u0989\u099F\u09AA\u09C1\u099F (stdout) \u09A5\u09C7\u0995\u09C7\
  \u2026"
lastmod: '2024-03-17T18:47:44.196548-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\
  \u09A1 \u098F\u09B0\u09B0 (stderr) \u09A4\u09C7 \u09B2\u09C7\u0996\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u09B9\u09B2 \u098F\u09B0\u09B0 \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE\
  \ \u098F\u09AC\u0982 \u09A1\u09BE\u09DF\u09BE\u0997\u09A8\u09B8\u09CD\u099F\u09BF\
  \u0995 \u0986\u0989\u099F\u09AA\u09C1\u099F\u0997\u09C1\u09B2\u09BF \u098F\u0995\
  \u099F\u09BF \u0986\u09B2\u09BE\u09A6\u09BE \u099A\u09CD\u09AF\u09BE\u09A8\u09C7\
  \u09B2\u09C7 \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09BF\u09A4 \u0995\u09B0\u09BE\
  , \u09AF\u09BE \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\
  \u09CD\u09A1 \u0986\u0989\u099F\u09AA\u09C1\u099F (stdout) \u09A5\u09C7\u0995\u09C7\
  \u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কি এবং কেন?
স্ট্যান্ডার্ড এরর (stderr) তে লেখা মানে হল এরর বার্তা এবং ডায়াগনস্টিক আউটপুটগুলি একটি আলাদা চ্যানেলে পরিচালিত করা, যা স্ট্যান্ডার্ড আউটপুট (stdout) থেকে পৃথক। প্রোগ্রামাররা এটি করেন সাধারণ প্রোগ্রামের ফলাফল এবং এরর তথ্যকে পৃথক করার জন্য, ডিবাগিং এবং লগিং প্রক্রিয়া সুষ্ঠু করার জন্য।

## কিভাবে:
Lua তে, stderr এ লেখা সাধিত হয় `io.stderr:write()` ফাংশন ব্যবহার করে। এখানে আপনি কীভাবে স্ট্যান্ডার্ড এররে একটি সাধারণ এরর বার্তা লিখতে পারেন তা দেখানো হয়েছে:

```lua
io.stderr:write("Error: Invalid input.\n")
```

যদি আপনার একটি ভেরিয়েবল বা একাধিক ডাটা পিস আউটপুট করার প্রয়োজন হয়, তাদেরকে write ফাংশনের মধ্যে যোগ করুন:

```lua
local errorMessage = "Invalid input."
io.stderr:write("Error: " .. errorMessage .. "\n")
```

**stderr এ নমুনা আউটপুট:**
```
Error: Invalid input.
```

আরও জটিল সিনারিও বা বড় অ্যাপ্লিকেশনগুলির সাথে কাজ করার সময়, আপনি LuaLogging এর মতো তৃতীয়-পক্ষের লগিং লাইব্রেরিগুলি বিবেচনা করতে পারেন। LuaLogging এর সাথে, আপনি বিভিন্ন গন্তব্যে লগ নির্দেশ করতে পারেন, যার মধ্যে stderr অন্তর্ভুক্ত। এখানে একটি সংক্ষিপ্ত উদাহরণ:

প্রথমে, LuaLogging ইনস্টল করুন LuaRocks ব্যবহার করে:

```
luarocks install lualogging
```

তারপর, LuaLogging ব্যবহার করে stderr এ একটি এরর বার্তা লেখার জন্য:

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Error: Invalid input.")
```

এই পদ্ধতি আপনার অ্যাপ্লিকেশনে মানকৃত লগিং এর সুবিধা দেয়, সাথে লগ লেভেল (যেমন, ERROR, WARN, INFO) সেট করার সহজ এপিআই এর সাথে অতিরিক্ত নমনীয়তা প্রদান করে।
