---
title:                "স্ট্রিংকে লোয়ার কেসে রূপান্তর করা"
date:                  2024-03-17T17:46:28.231897-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

একটি স্ট্রিংকে লোয়ার কেসে রূপান্তর করা মানে একটি স্ট্রিং এর মধ্যে সকল অক্ষরকে তাদের লোয়ার কেস ফর্মে পরিবর্তন করা। প্রোগ্রামাররা ডাটা স্টোরেজ, তুলনা এবং খোঁজের সাথে সঙ্গতিপূর্ণ রাখার জন্য এটি করে থাকে।

## কিভাবে:

Elixir এটিকে সহজ করে দেয়। `String.downcase/1` ফাংশনটি ব্যবহার করুন:

```elixir
original = "LoReM IPSUM"
lowercased = String.downcase(original)

IO.puts original
IO.puts lowercased
```

আউটপুট:

```
LoReM IPSUM
lorem ipsum
```

## গভীর ডাইভ

Elixir এর স্ট্রিং হ্যান্ডলিং Unicode এর প্রতি সচেতন, যা বিভিন্ন অক্ষর এবং স্ক্রিপ্টে যথাযথ lower-casing এর জন্য অনেক গুরুত্বপূর্ণ। ঐতিহাসিকভাবে, প্রোগ্রামিং ভাষাগুলিতে স্ট্রিং ম্যানিপুলেশন সবসময় এই জটিলতাগুলি বিবেচনা করেনি।

Elixir এর বর্তমান পদ্ধতির আগে, কিছু পুরানো ভাষা সহজ পদ্ধতি প্রদান করত, যা ইংরেজির জন্য ঠিক থাকত, কিন্তু তুরস্কীয় ভাষার মতো অন্যান্য ভাষায় সমস্যা সৃষ্টি করত, যেখানে উদাহরণস্বরূপ, একটি বড় 'i' 'I' হয়ে যায় না বরং 'İ' হয়।

অভ্যন্তরীণভাবে, Elixir এই বিষয়টি ঠিক পাওয়ার জন্য Unicode এর case mapping ব্যবহার করে। এবং বিকল্প রয়েছে; উদাহরণস্বরূপ, `String.downcase/2` আপনাকে একটি লোকাল নির্দিষ্ট করতে দেয়, যা ভাষা-নির্দিষ্ট আচরণের জন্য কাজে লাগে।

```elixir
turkish = "GÖLCÜK"
String.downcase(turkish, :tr)
```

আউটপুট:

```
gölcük
```

উপরের উদাহরণে, লক্ষ্য করুন কিভাবে 'I' অক্ষর তুরস্কীয় কেসিং নিয়ম অনুযায়ী যথাযথভাবে সংরক্ষিত হয়।

## দেখুন ও

- Elixir এর অফিসিয়াল `String` মডিউল ডকুমেন্টেশন: https://hexdocs.pm/elixir/String.html
- Unicode case mapping: https://www.unicode.org/reports/tr21/tr21-5.html
- Elixir এ Unicode সম্পর্কে একটি দ্রুত গাইড: https://elixir-lang.org/blog/2017/01/05/elixir-and-unicode-part-1/
