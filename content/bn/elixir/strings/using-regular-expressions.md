---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:26:08.137654-06:00
description: "Elixir-\u098F \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\u09A4 \u0985\u09AD\
  \u09BF\u09AC\u09CD\u09AF\u0995\u09CD\u09A4\u09BF (regex) \u09AC\u09BF\u09B6\u09C7\
  \u09B7 \u09A8\u09BF\u09A6\u09B0\u09CD\u09B6\u09A8 \u0985\u09A8\u09C1\u09AF\u09BE\
  \u09AF\u09BC\u09C0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0996\u09CB\u0981\
  \u099C\u09BE, \u09AE\u09BF\u09B2\u09BE\u09A8\u09CB, \u098F\u09AC\u0982 \u09AA\u09B0\
  \u09BF\u09AC\u09B0\u09CD\u09A4\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u09B9\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09B2\u09BF\u0982\
  -\u098F \u098F\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.657191-06:00'
model: gpt-4-0125-preview
summary: "Elixir-\u098F \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\u09A4 \u0985\u09AD\u09BF\
  \u09AC\u09CD\u09AF\u0995\u09CD\u09A4\u09BF (regex) \u09AC\u09BF\u09B6\u09C7\u09B7\
  \ \u09A8\u09BF\u09A6\u09B0\u09CD\u09B6\u09A8 \u0985\u09A8\u09C1\u09AF\u09BE\u09AF\
  \u09BC\u09C0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0996\u09CB\u0981\u099C\
  \u09BE, \u09AE\u09BF\u09B2\u09BE\u09A8\u09CB, \u098F\u09AC\u0982 \u09AA\u09B0\u09BF\
  \u09AC\u09B0\u09CD\u09A4\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09B9\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09B2\u09BF\u0982-\u098F\
  \ \u098F\u09B0\u2026"
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

## কি এবং কেন?

Elixir-এ নিয়মিত অভিব্যক্তি (regex) বিশেষ নিদর্শন অনুযায়ী স্ট্রিং খোঁজা, মিলানো, এবং পরিবর্তনের জন্য ব্যবহৃত হয়। প্রোগ্রামাররা স্ট্রিং হ্যান্ডলিং-এ এর দক্ষতা এবং বহুমুখিতার কারণে ইমেইল, URLs এর মতো বিন্যাস যাচাই করা, লগ পার্স করা, অথবা তথ্য প্রক্ষেপণ এর মতো কাজে regex-কে কাজে লাগায়।

## কিভাবে:

Elixir নিয়মিত অভিব্যক্তি অপারেশনগুলির জন্য `Regex` মডিউলটি ব্যবহার করে, যা Erlang-এর regex লাইব্রেরির আধারে। এখানে মৌলিক ব্যবহার দেওয়া হল:

```elixir
# একটি নিদর্শন মেলে - প্রথম মিল ফেরত দেয়
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # আউটপুট: ["hello"]

# সকল মিল খুঁজে বের করা
all_matches = Regex.scan(~r/\d/, "There are 2 apples and 5 oranges.")
IO.inspect(all_matches) # আউটপুট: [["2"], ["5"]]

# একটি স্ট্রিং-এর অংশ প্রতিস্থাপন
replaced_string = Regex.replace(~r/\s+/, "Elixir is fun", "_")
IO.inspect(replaced_string) # আউটপুট: "Elixir_is_fun"
```

আরও জটিল নিদর্শন এবং কার্যক্ষমতার জন্য, আপনি তৃতীয় পক্ষের লাইব্রেরি ব্যবহার করতে পারেন, যদিও সর্বাধিক মূল স্ট্রিং এবং নিদর্শন মিলানোর কাজের জন্য, Elixir-এর নির্মিত `Regex` মডিউল বেশ শক্তিশালী।

কেস-অসংবেদনশীল ম্যাচের জন্য, `i` অপশন ব্যবহার করুন:

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # আউটপুট: ["Hello"]
```

যদি একাধিকবার ব্যবহৃত হয়, দক্ষতা বৃদ্ধির জন্য regex এক্সপ্রেশনগুলি পূর্ব-সম্পাদিত করা সম্ভব:

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # আউটপুট: ["hello"]
```

Elixir-এ নামযুক্ত ক্যাপচারগুলি সমর্থন করে, যা স্ট্রিং-এর বিশেষ অংশগুলি প্রক্ষেপণ করতে এবং আপনার কোডকে আরও পাঠযোগ্য করতে খুবই সুবিধাজনক হতে পারে:

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # আউটপুট: %{"year" => "2023", "month" => "04", "day" => "15"}
```

নিয়মিত অভিব্যক্তির সহজ ব্যবহার করে যেভাবে Elixir শক্তিশালী স্ট্রিং ম্যানিপুলেশন এবং ডেটা প্রক্ষেপণের কৌশল সম্ভব করে তোলে, তা এই সংক্ষিপ্ত সারসংকলনে উল্লেখ করা হয়েছে।
