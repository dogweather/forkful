---
title:                "রেগুলার এক্সপ্রেশন ব্যবহার করা"
date:                  2024-03-17T18:26:08.137654-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
