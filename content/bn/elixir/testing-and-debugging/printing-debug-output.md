---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:06:59.411607-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u099F\u09BF \u0995\u09A8\
  \u09B8\u09CB\u09B2\u09C7 \u0995\u09BF\u099B\u09C1 \u09AA\u09CD\u09B0\u09BF\u09A8\
  \u09CD\u099F \u0995\u09B0\u09BE\u09B0 \u09B8\u09B9\u099C\u09A4\u09AE \u0989\u09AA\
  \u09BE\u09AF\u09BC \u09A6\u09C7\u0996\u09BE\u09AF\u09BC `IO.inspect/2` \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964 \u09B2\u09C7\u09AC\u09C7\
  \u09B2 \u0985\u09AA\u09B6\u09A8 \u0995\u09BE\u09B8\u09CD\u099F\u09AE \u09AA\u09CD\
  \u09B0\u09BF\u09AB\u09BF\u0995\u09CD\u09B8 \u09AF\u09CB\u0997 \u0995\u09B0\u09C7\
  , \u0986\u0989\u099F\u09AA\u09C1\u099F\u0995\u09C7 \u099A\u09BF\u09A8\u09A4\u09C7\
  \ \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964."
lastmod: '2024-04-05T21:53:51.751357-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u099F\u09BF \u0995\u09A8\u09B8\u09CB\u09B2\u09C7 \u0995\u09BF\u099B\
  \u09C1 \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE\u09B0 \u09B8\
  \u09B9\u099C\u09A4\u09AE \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u0996\u09BE\
  \u09AF\u09BC `IO.inspect/2` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7\u0964 \u09B2\u09C7\u09AC\u09C7\u09B2 \u0985\u09AA\u09B6\u09A8 \u0995\u09BE\
  \u09B8\u09CD\u099F\u09AE \u09AA\u09CD\u09B0\u09BF\u09AB\u09BF\u0995\u09CD\u09B8\
  \ \u09AF\u09CB\u0997 \u0995\u09B0\u09C7, \u0986\u0989\u099F\u09AA\u09C1\u099F\u0995\
  \u09C7 \u099A\u09BF\u09A8\u09A4\u09C7 \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A4\
  \u09CB\u09B2\u09C7\u0964."
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কিভাবে:
```elixir
defmodule DebugExample do
  def show_debug_output do
    name = "Elixir"

    IO.inspect(name, label: "Debug")
    # আরো প্রসেসিং
  end
end

DebugExample.show_debug_output()
# আউটপুট:
# Debug: "Elixir"
```

এটি কনসোলে কিছু প্রিন্ট করার সহজতম উপায় দেখায় `IO.inspect/2` ব্যবহার করে। লেবেল অপশন কাস্টম প্রিফিক্স যোগ করে, আউটপুটকে চিনতে সহজ করে তোলে।

## গভীর নিবন্ধ
Elixir এর `IO.inspect/2` ফাংশন হল Ruby তে `puts` অথবা JavaScript এ `console.log` এর অনুরূপ। এটি দ্রুত-এবং-ময়লা ডিবাগিং এর জন্য দারুণ, যা প্রোগ্রামিং নিজের শুরু থেকে একটি চর্চার বিষয়।

Elixir এর বিকল্পগুলোর মধ্যে আরো সিস্টেম্যাটিক অ্যাপলিকেশন-লেভেল লগিং এর জন্য `Logger` মডিউল ব্যবহার রয়েছে। এটি আরো কনফিগারেবল এবং প্রোডাকশনের জন্য উপযুক্ত।

বাস্তবায়ন বিস্তারিত হিসেবে, `IO.inspect/2` দেওয়া ডেটা ফেরত দেয়, এটি কোনো কার্যকারিতা প্রভাবিত না করে একটি পাইপলাইনে সন্নিবেশ করা সহজ করে। ঐতিহাসিকভাবে, Elixir সর্বদা ডেভেলপার টুলিং এর উপর জোর দিয়েছে, এবং ফাংশনের মত `IO.inspect/2` এটি ডিবাগিংকে একটি আরো সংহত অভিজ্ঞতা দ্বারা উজ্জীবিত করে থাকে।

## দেখুন আরো
- Elixir এর IO মডিউল: https://hexdocs.pm/elixir/IO.html
- Elixir এ ডিবাগিং পরিচিতি: https://elixirschool.com/en/lessons/specifics/debugging
- Logger এর অফিশিয়াল গাইড: https://hexdocs.pm/logger/Logger.html
