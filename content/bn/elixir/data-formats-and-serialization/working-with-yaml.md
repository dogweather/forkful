---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:02.426421-06:00
description: "YAML, \u09AF\u09BE 'YAML Ain't Markup Language' \u098F\u09B0 \u09B8\u0982\
  \u0995\u09CD\u09B7\u09BF\u09AA\u09CD\u09A4 \u09B0\u09C2\u09AA, \u098F\u0995\u099F\
  \u09BF \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09CD\u09AF \u09A1\u09C7\u099F\
  \u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\
  \u09A8 \u09AE\u09BE\u09A8\u09A6\u09A3\u09CD\u09A1 \u09AF\u09BE \u09B8\u09BE\u09A7\
  \u09BE\u09B0\u09A3\u09A4 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8\
  \ \u09AB\u09BE\u0987\u09B2 \u098F\u09AC\u0982 \u09AD\u09BF\u09A8\u09CD\u09A8 \u09A1\
  \u09C7\u099F\u09BE \u0995\u09BE\u09A0\u09BE\u09AE\u09CB \u09B8\u09B9\u2026"
lastmod: '2024-03-17T18:47:43.692822-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u09AF\u09BE 'YAML Ain't Markup Language' \u098F\u09B0 \u09B8\u0982\
  \u0995\u09CD\u09B7\u09BF\u09AA\u09CD\u09A4 \u09B0\u09C2\u09AA, \u098F\u0995\u099F\
  \u09BF \u09AE\u09BE\u09A8\u09AC-\u09AA\u09BE\u09A0\u09CD\u09AF \u09A1\u09C7\u099F\
  \u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\
  \u09A8 \u09AE\u09BE\u09A8\u09A6\u09A3\u09CD\u09A1 \u09AF\u09BE \u09B8\u09BE\u09A7\
  \u09BE\u09B0\u09A3\u09A4 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8\
  \ \u09AB\u09BE\u0987\u09B2 \u098F\u09AC\u0982 \u09AD\u09BF\u09A8\u09CD\u09A8 \u09A1\
  \u09C7\u099F\u09BE \u0995\u09BE\u09A0\u09BE\u09AE\u09CB \u09B8\u09B9\u2026"
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

YAML, যা 'YAML Ain't Markup Language' এর সংক্ষিপ্ত রূপ, একটি মানব-পাঠ্য ডেটা সিরিয়ালাইজেশন মানদণ্ড যা সাধারণত কনফিগারেশন ফাইল এবং ভিন্ন ডেটা কাঠামো সহ ভাষার মধ্যে ডেটা বিনিময়ের জন্য ব্যবহৃত হয়। প্রোগ্রামাররা এর সরলতা এবং জটিল হাইয়ারারকিক্যাল ডেটাকে সহজে উপস্থাপন করার ক্ষমতার জন্য এটি ব্যবহার করেন।

## কিভাবে:

Elixir বিল্ট-ইন YAML সাপোর্ট অন্তর্ভুক্ত করে না। তবে, আপনি `yamerl` বা `yaml_elixir` এর মত থার্ড-পার্টি লাইব্রেরিগুলি ব্যবহার করে YAML এর সাথে কাজ করতে পারেন। এখানে, আমরা `yaml_elixir` এর উপর ফোকাস করব এর ব্যবহারের সহজতা এবং ব্যাপক বৈশিষ্ট্যের জন্য।

প্রথমে, আপনার mix.exs ডিপেনডেন্সিগুলিতে `yaml_elixir` যোগ করুন:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

তারপর, নতুন নির্ভরতা আনতে `mix deps.get` চালান।

### YAML পড়া

একটি সহজ YAML ফাইল, `config.yaml`, যা এরকম দেখতে:

```yaml
database:
  adapter: postgres
  username: user
  password: pass
```

আপনি এই YAML ফাইলটি পড়তে এবং এটিকে Elixir ম্যাপে পরিণত করতে পারেন এইরকম:

```elixir
defmodule Config do
  def read do
    {:ok, content} = YamlElixir.read_from_file("config.yaml")
    content
  end
end

# নমুনা ব্যবহার
Config.read()
# আউটপুট: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "user",
#     "password" => "pass"
#   }
# }
```

### YAML লেখা

একটি ম্যাপকে ফেরত YAML ফাইলে লিখতে:

```elixir
defmodule ConfigWriter do
  def write do
    content = %{
      database: %{
        adapter: "mysql",
        username: "root",
        password: "s3cret"
      }
    }
    
    YamlElixir.write_to_file("new_config.yaml", content)
  end
end

# নমুনা ব্যবহার
ConfigWriter.write()
# এটি `new_config.yaml` নির্দিষ্ট কন্টেন্ট দিয়ে তৈরি বা ওভাররাইট করবে
```

লক্ষ করুন কিভাবে `yaml_elixir` YAML ফাইল এবং Elixir ডেটা কাঠামোর মধ্যে একটি সরাসরি অনুবাদ সম্ভব করে তোলে, যা এটিকে YAML ডেটা নিয়ে কাজ করতে ইচ্ছুক Elixir প্রোগ্রামারদের জন্য একটি চমৎকার পছন্দ করে তোলে।
