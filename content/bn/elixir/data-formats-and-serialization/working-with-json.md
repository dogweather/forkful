---
title:                "JSON এর সাথে কাজ করা"
date:                  2024-03-17T18:29:12.556233-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

JSON নিয়ে কাজ করা মানে JSON-ফর্ম্যাটে লেখা স্ট্রিংগুলোকে Elixir এর ডাটা স্ট্রাকচারে পরিণত করা, এবং Elixir ডাটা স্ট্রাকচারগুলোকে আবার JSON স্ট্রিং এ পরিণত করা। ওয়েব ডেভেলপমেন্ট, API, এবং কনফিগারেশন ফাইলগুলোর জন্য এটি অপরিহার্য, কারণ JSON হল একটি হালকা, টেক্সট-ভিত্তিক, ভাষা-স্বাধীন ডাটা এক্সচেঞ্জ ফর্ম্যাট যা এর সাদাসিদে এবং মানব পাঠযোগ্যতার জন্য বিস্তৃতভাবে ব্যবহৃত হয়।

## কিভাবে:

Elixir এ, আপনি JSON পার্সিং এবং জেনারেশনের জন্য `Jason` লাইব্রেরি ব্যবহার করতে পারেন, যা একটি জনপ্রিয় পছন্দ। প্রথমে, `mix.exs` এ আপনার প্রজেক্টের নির্ভরতা হিসেবে `Jason` যোগ করুনঃ

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

তারপর, নির্ভরতা আনার জন্য `mix deps.get` কমান্ড চালান।

### JSON পার্সিংঃ
JSON স্ট্রিংকে Elixir ডাটা স্ট্রাকচারে রূপান্তর করতেঃ

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# আউটপুট: %{"name" => "John", "age" => 30}
```

### JSON জেনারেট করা:
Elixir ম্যাপকে JSON স্ট্রিং এ রূপান্তর করতেঃ

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# আউটপুট: {"age":25,"name":"Jane"}
```

### স্ট্রাকচারের সাথে কাজ করা:
Elixir স্ট্রাকচার এনকোড করতে, আপনাকে আপনার স্ট্রাকচারের জন্য `Jason.Encoder` প্রোটোকল বাস্তবায়ন করতে হবে। এখানে একটি উদাহরণ দেওয়া হলঃ

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# আউটপুট: {"age":28,"name":"Mike"}
```

এই সহজ পদ্ধতি আপনার Elixir অ্যাপ্লিকেশনে JSON প্রসেসিং একীভূত করা শুরু করার উপর আপনাকে পথ নির্দেশনা দিবে, বিভিন্ন প্রোগ্রামিং পরিবেশে ডাটা ইন্টারচেঞ্জ সুবিধা প্রদান করবে।
