---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:24.319058-06:00
description: "Elixir \u098F \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\
  \u09BE\u09B0\u09CD\u09A1 \u098F\u09B0\u09B0 (stderr) \u098F \u09B2\u09C7\u0996\u09BE\
  \u09B0 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09B9\u09B2 \u098F\u09B0\u09B0 \u09AE\
  \u09C7\u09B8\u09C7\u099C \u098F\u09AC\u0982 \u09A1\u09BE\u09DF\u09BE\u0997\u09A8\
  \u09B8\u09CD\u099F\u09BF\u0995\u09B8 \u0995\u09C7 \u09AE\u09C2\u09B2 \u0986\u0989\
  \u099F\u09AA\u09C1\u099F (stdout) \u09A5\u09C7\u0995\u09C7 \u0986\u09B2\u09BE\u09A6\
  \u09BE \u0995\u09B0\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6 \u0995\u09B0\
  \u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE stderr\u2026"
lastmod: '2024-03-17T18:47:43.688071-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u098F \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\
  \u09B0\u09CD\u09A1 \u098F\u09B0\u09B0 (stderr) \u098F \u09B2\u09C7\u0996\u09BE\u09B0\
  \ \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09B9\u09B2 \u098F\u09B0\u09B0 \u09AE\u09C7\
  \u09B8\u09C7\u099C \u098F\u09AC\u0982 \u09A1\u09BE\u09DF\u09BE\u0997\u09A8\u09B8\
  \u09CD\u099F\u09BF\u0995\u09B8 \u0995\u09C7 \u09AE\u09C2\u09B2 \u0986\u0989\u099F\
  \u09AA\u09C1\u099F (stdout) \u09A5\u09C7\u0995\u09C7 \u0986\u09B2\u09BE\u09A6\u09BE\
  \ \u0995\u09B0\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6 \u0995\u09B0\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ stderr\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
---

{{< edit_this_page >}}

## কি ও কেন?

Elixir এ স্ট্যান্ডার্ড এরর (stderr) এ লেখার পদ্ধতি হল এরর মেসেজ এবং ডায়াগনস্টিকস কে মূল আউটপুট (stdout) থেকে আলাদা করে নির্দেশ করা। প্রোগ্রামাররা stderr ব্যবহার করে এরর ডিবাগ এবং হ্যান্ডল করে, যাতে প্রোগ্রামের মূল আউটপুট নোংরা না হয়, এবং এতে এরর চিহ্নিত এবং সমাধান করা সহজ হয়।

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
