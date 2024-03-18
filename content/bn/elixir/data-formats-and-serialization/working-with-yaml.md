---
title:                "ইয়ামেল নিয়ে কাজ করা"
date:                  2024-03-17T18:37:02.426421-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
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
