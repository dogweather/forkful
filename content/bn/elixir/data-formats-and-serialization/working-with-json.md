---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:12.556233-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elixir \u098F, \u0986\u09AA\u09A8\
  \u09BF JSON \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u098F\u09AC\u0982 \u099C\
  \u09C7\u09A8\u09BE\u09B0\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ `Jason` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\
  , \u09AF\u09BE \u098F\u0995\u099F\u09BF \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\
  \u09BC \u09AA\u099B\u09A8\u09CD\u09A6\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7\
  , `mix.exs` \u098F \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u099C\u09C7\
  \u0995\u09CD\u099F\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.693884-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u098F, \u0986\u09AA\u09A8\u09BF JSON \u09AA\u09BE\u09B0\u09CD\u09B8\
  \u09BF\u0982 \u098F\u09AC\u0982 \u099C\u09C7\u09A8\u09BE\u09B0\u09C7\u09B6\u09A8\
  \u09C7\u09B0 \u099C\u09A8\u09CD\u09AF `Jason` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8, \u09AF\u09BE \u098F\u0995\u099F\u09BF \u099C\
  \u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09AA\u099B\u09A8\u09CD\u09A6\u0964\
  \ \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, `mix.exs` \u098F \u0986\u09AA\u09A8\u09BE\
  \u09B0 \u09AA\u09CD\u09B0\u099C\u09C7\u0995\u09CD\u099F\u09C7\u09B0 \u09A8\u09BF\
  \u09B0\u09CD\u09AD\u09B0\u09A4\u09BE \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 `Jason`\
  \ \u09AF\u09CB\u0997 \u0995\u09B0\u09C1\u09A8\u0983."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

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
