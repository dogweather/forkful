---
title:                "HTML পার্স করা"
date:                  2024-03-17T18:03:58.649113-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

Elixir-এ HTML পার্স করা মানে হল HTML ডকুমেন্ট থেকে তথ্য বের করা। প্রোগ্রামাররা এটা করে ওয়েব পেজগুলির সাথে প্রোগ্রামেটিকভাবে ইন্টারাক্ট করার জন্য, ডেটা স্ক্র্যাপ করার জন্য, অথবা ওয়েব ইন্টারাকশনের অটোমেশন করার জন্য, যা অ্যাপ্লিকেশনগুলিকে ডাইনামিকভাবে ওয়েব কনটেন্ট বুঝতে এবং ব্যবহার করতে সক্ষম করে।

## কিভাবে:

Elixir, এর শক্তিশালী সামান্যতা মডেল এবং ফাংশনাল প্রোগ্রামিং পরিপ্রেক্ষিত সত্ত্বেও, HTML পার্সিং সক্ষমতার অন্তর্ভুক্ত করে না। তবে, আপনি এর জন্য `Floki` এর মত জনপ্রিয় থার্ড-পার্টি লাইব্রেরি ব্যবহার করতে পারেন। Floki HTML পার্সিংকে ইন্টুইটিভ এবং কার্যকর করে তোলে, Elixir-এর প্যাটার্ন ম্যাচিং এবং পাইপিং বৈশিষ্ট্যগুলি ব্যবহার করে।

প্রথমে, Floki-কে আপনার mix.exs dependencies-এ যোগ করুন:

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

তারপর, `mix deps.get` রান করে নতুন নির্ভরতা ইনস্টল করুন।

এখন, আসুন আমরা একটি সহজ HTML স্ট্রিং পার্স করে তথ্য বের করি। আমরা `<h1>` ট্যাগের মধ্যে থাকা শিরোনামগুলি খুঁজব:

```elixir
html_content = """
<html>
  <body>
    <h1>Hello, Elixir!</h1>
    <h1>Another Title</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**স্যাম্পল আউটপুট:**

```elixir
["Hello, Elixir!", "Another Title"]
```

আরও গভীরে যেতে চাইলে, ধরুন আপনি লিঙ্ক (`<a>` ট্যাগস্) এবং তাদের href অ্যাট্রিবিউটগুলি বের করতে চান। এখানে কিভাবে আপনি এটি অর্জন করতে পারেন:

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">Elixir's Official Website</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**স্যাম্পল আউটপুট:**

```elixir
[{"Elixir's Official Website", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

এই পদ্ধতি আপনাকে HTML ডকুমেন্টগুলি দক্ষভাবে নেভিগেট এবং পার্স করতে সাহায্য করে, Elixir অ্যাপ্লিকেশনগুলিতে ওয়েব ডেটা এক্সট্রাকশন এবং ম্যানিপুলেশন টাস্কগুলি সোজা করে দেয়।
