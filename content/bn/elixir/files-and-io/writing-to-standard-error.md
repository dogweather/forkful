---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:24.319058-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elixir \u098F, \u0986\u09AA\u09A8\
  \u09BF `IO` \u09AE\u09A1\u09BF\u0989\u09B2\u09C7\u09B0 \u09AB\u09BE\u0982\u09B6\u09A8\
  \ \u09AF\u09C7\u09AE\u09A8 `IO.puts/2` \u098F\u09AC\u0982 `IO.warn/2` \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\
  \u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u098F\u09B0\u09B0\u09C7 \u09AE\
  \u09C7\u09B8\u09C7\u099C \u09B2\u09C7\u0996\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\
  \u09A8."
lastmod: '2024-03-17T18:47:43.688071-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u098F, \u0986\u09AA\u09A8\u09BF `IO` \u09AE\u09A1\u09BF\u0989\u09B2\
  \u09C7\u09B0 \u09AB\u09BE\u0982\u09B6\u09A8 \u09AF\u09C7\u09AE\u09A8 `IO.puts/2`\
  \ \u098F\u09AC\u0982 `IO.warn/2` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\
  \u09A1 \u098F\u09B0\u09B0\u09C7 \u09AE\u09C7\u09B8\u09C7\u099C \u09B2\u09C7\u0996\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কিভাবে:
Elixir এ, আপনি `IO` মডিউলের ফাংশন যেমন `IO.puts/2` এবং `IO.warn/2` ব্যবহার করে স্ট্যান্ডার্ড এররে মেসেজ লেখতে পারেন:

```elixir
# stderr এ একটি সাধারন মেসেজ লেখা
IO.puts(:stderr, "Error: Something went wrong!")

# ওয়ার্নিং/এরারের জন্য আরও মানেবাচক IO.warn ব্যবহার করা
IO.warn("Warning: You are about to exceed the limit!")
```

`IO.puts/2` এর জন্য টার্মিনালে নমুনা আউটপুট হবে:
```
Error: Something went wrong!
```

`IO.warn/2` এর জন্য, আউটপুট অনুরূপ হবে, তবে `IO.warn/2` বিশেষভাবে ওয়ার্নিং এর জন্য ডিজাইন করা হয়েছে এবং ভবিষ্যতে এলিক্সির ভার্সনগুল৤তে অতিরিক্ত ফর্ম্যাটিং বা আচরণ অন্তর্ভুক্ত করা হতে পারে।

**তৃতীয়-পক্ষের লাইব্রেরী ব্যবহার করা**

যদিও Elixir এর মান লাইব্রেরী সাধারণত স্ট্যান্ডার্ড এরর আউটপুট হ্যান্ডল করার জন্য যথেষ্ট, আপনি আরও জটিল অ্যাপ্লিকেশন বা বিভিন্ন লগ লেভেল এবং আউটপুট কনফিগার করার জন্য যেমন `Logger` মতো লাইব্রেরী উপযোগী পেতে পারেন।

একটি এরর মেসেজ আউটপুট করার জন্য `Logger` ব্যবহারের উদাহরণ:

```elixir
require Logger

# Logger কে stderr এ আউটপুট করার জন্য কনফিগার করা
Logger.configure_backend(:console, device: :stderr)

# একটি এরর মেসেজ লেখা
Logger.error("Error: Failed to connect to the database.")
```

এই সেটআপ `Logger` এর আউটপুটকে বিশেষভাবে stderr এ নির্দেশ করে, যা স্ট্যান্ডার্ড লগ বার্তা থেকে এরর লগিং আলাদা করার জন্য উপযোগী।
