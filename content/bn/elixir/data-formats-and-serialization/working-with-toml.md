---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:33.475978-06:00
description: "Elixir \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ TOML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u09B9\u09B2 TOML (Tom's Obvious, Minimal Language) \u09A1\u09BE\
  \u099F\u09BE \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u098F\u09AC\u0982\
  \ \u099C\u09C7\u09A8\u09BE\u09B0\u09C7\u099F \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0995\u09A8\u09AB\
  \u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2\u2026"
lastmod: '2024-03-17T18:47:43.696509-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 TOML\
  \ \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u09B9\u09B2 TOML (Tom's Obvious, Minimal Language) \u09A1\u09BE\u099F\
  \u09BE \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u099C\
  \u09C7\u09A8\u09BE\u09B0\u09C7\u099F \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0995\u09A8\u09AB\u09BF\
  \u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2 \u09B9\u09CD\u09AF\
  \u09BE\u09A8\u09CD\u09A1\u09B2 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \u09A8 \u0995\u09BE\u09B0\u09A3 TOML \u09AA\u09A1\u09BC\u09BE \u09B8\u09B9\u099C\
  , \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u09B8\u09B9\u099C \u098F\u09AC\
  \u0982 \u098F\u099F\u09BF \u098F\u0995\u099F\u09BF \u09B9\u09CD\u09AF\u09BE\u09B6\
  \ \u09A1\u09BE\u099F\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BE\u0995\u099A\u09BE\
  \u09B0\u09C7\u09B0 \u09B8\u0999\u09CD\u0997\u09C7 \u09AD\u09BE\u09B2 \u09AE\u09CD\
  \u09AF\u09BE\u09AA \u0995\u09B0\u09C7\u0964."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কিভাবে:
প্রথমে, আপনার mix ডিপেন্ডেন্সিগুলিতে একটি TOML parser যোগ করুন। এই উদাহরণটি `toml-elixir` ব্যবহার করে:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

একটি TOML ফাইল পড়ুন:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

Elixir ডাটাকে TOML এ রূপান্তর করতে:

```elixir
data = %{title: "TOML Example", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

নমুনা আউটপুট:

```elixir
"title = \"TOML Example\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## গভীর ডুব
TOML কনফিগারেশন ফাইলের জন্য ব্যবহারের জন্য Tom Preston-Werner, GitHub-এর সহ-প্রতিষ্ঠাতার দ্বারা তৈরি করা হয়েছিল। এটি ডিজাইন করা হয়েছিল XML এর চেয়ে সরল এবং YAML এর চেয়ে সংক্ষিপ্ত হওয়ার সাথে সাথে ধারাবাহিকতা বজায় রাখার জন্য।

বিকল্পগুলি অন্তর্ভুক্ত করে JSON, YAML, এবং INI ফাইল, প্রতিটির মানব পাঠযোগ্যতা এবং ডাটা স্ট্রাকচার সামঞ্জস্যে তাদের নিজস্ব বিনিময়। TOML টেবিলার ডাটা এবং নেস্টেড ডাটা গ্রুপিং স্পষ্টভাবে প্রতিনিধিত্বে সেরা করে তোলে।

Elixir-এ, TOML হ্যান্ডলিং ডিকোডিং এবং এনকোডিং লাইব্রেরিগুলিতে নির্ভর করে, যা TOML স্ট্রিংগুলিকে Elixir ম্যাপগুলিতে এবং বিপরীতে রূপান্তরিত করে। পার্সিং TOML-এর সিনট্যাক্স নিয়মগুলির সাথে মিলে যায় এবং Elixir-এর ডেটা টাইপগুলিতে রূপান্তর করে। এনকোডিং এর বিপরীত হয়, এটি Elixir-এর ডেটা টাইপগুলিকে বৈধ TOML সিনট্যাক্সে ম্যাপ করে।

## আরও দেখুন
- TOML ভাষা: https://toml.io/en/
- `toml-elixir` GitHub রিপোজিটরি: https://github.com/bitwalker/toml-elixir
- `toml-elixir`ের Hex প্যাকেজের বিস্তারিত: https://hex.pm/packages/toml_elixir
