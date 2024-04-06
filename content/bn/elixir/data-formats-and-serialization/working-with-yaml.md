---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:02.426421-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elixir \u09AC\u09BF\u09B2\u09CD\
  \u099F-\u0987\u09A8 YAML \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u0985\u09A8\
  \u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09C7 \u09A8\
  \u09BE\u0964 \u09A4\u09AC\u09C7, \u0986\u09AA\u09A8\u09BF `yamerl` \u09AC\u09BE\
  \ `yaml_elixir` \u098F\u09B0 \u09AE\u09A4 \u09A5\u09BE\u09B0\u09CD\u09A1-\u09AA\u09BE\
  \u09B0\u09CD\u099F\u09BF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\
  \u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ YAML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C\u2026"
lastmod: '2024-04-05T21:53:51.783300-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 YAML \u09B8\u09BE\u09AA\
  \u09CB\u09B0\u09CD\u099F \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\
  \u09A4 \u0995\u09B0\u09C7 \u09A8\u09BE\u0964 \u09A4\u09AC\u09C7, \u0986\u09AA\u09A8\
  \u09BF `yamerl` \u09AC\u09BE `yaml_elixir` \u098F\u09B0 \u09AE\u09A4 \u09A5\u09BE\
  \u09B0\u09CD\u09A1-\u09AA\u09BE\u09B0\u09CD\u099F\u09BF \u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7 YAML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7, \u0986\u09AE\u09B0\u09BE `yaml_elixir` \u098F\u09B0 \u0989\
  \u09AA\u09B0 \u09AB\u09CB\u0995\u09BE\u09B8 \u0995\u09B0\u09AC \u098F\u09B0 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u09B8\u09B9\u099C\u09A4\u09BE\
  \ \u098F\u09AC\u0982 \u09AC\u09CD\u09AF\u09BE\u09AA\u0995 \u09AC\u09C8\u09B6\u09BF\
  \u09B7\u09CD\u099F\u09CD\u09AF\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\u0964 \u09AA\
  \u09CD\u09B0\u09A5\u09AE\u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 mix.exs \u09A1\u09BF\
  \u09AA\u09C7\u09A8\u09A1\u09C7\u09A8\u09CD\u09B8\u09BF\u0997\u09C1\u09B2\u09BF\u09A4\
  \u09C7 `yaml_elixir` \u09AF\u09CB\u0997 \u0995\u09B0\u09C1\u09A8."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

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
