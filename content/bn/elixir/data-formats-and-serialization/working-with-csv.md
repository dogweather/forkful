---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:51.259097-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elixir \u09A4\u09BE\u09B0 \u09B6\
  \u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\
  \u09B0\u09CD\u09A8 \u09AE\u09CD\u09AF\u09BE\u099A\u09BF\u0982 \u098F\u09AC\u0982\
  \ \u09AA\u09BE\u0987\u09AA\u09B2\u09BE\u0987\u09A8\u09BF\u0982 \u09B8\u09AE\u09B0\
  \u09CD\u09A5\u09A8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7, \u09AC\
  \u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u09A5\u09BE\u09B0\u09CD\u09A1-\u09AA\u09BE\
  \u09B0\u09CD\u099F\u09BF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \ \u099B\u09BE\u09A1\u09BC\u09BE\u0987 CSV \u09AB\u09BE\u0987\u09B2 \u09A6\u0995\
  \u09CD\u09B7\u09AD\u09BE\u09AC\u09C7 \u09B9\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09C7\
  \u09B2 \u0995\u09B0\u09A4\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.695086-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u09A4\u09BE\u09B0 \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\
  \u09C0 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u09AE\u09CD\u09AF\
  \u09BE\u099A\u09BF\u0982 \u098F\u09AC\u0982 \u09AA\u09BE\u0987\u09AA\u09B2\u09BE\
  \u0987\u09A8\u09BF\u0982 \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8\u09C7\u09B0 \u09AE\
  \u09BE\u09A7\u09CD\u09AF\u09AE\u09C7, \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995\
  \ \u09A5\u09BE\u09B0\u09CD\u09A1-\u09AA\u09BE\u09B0\u09CD\u099F\u09BF \u09B2\u09BE\
  \u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u099B\u09BE\u09A1\u09BC\u09BE\u0987\
  \ CSV \u09AB\u09BE\u0987\u09B2 \u09A6\u0995\u09CD\u09B7\u09AD\u09BE\u09AC\u09C7\
  \ \u09B9\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09C7\u09B2 \u0995\u09B0\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7\u0964 \u09A4\u09AC\u09C7, \u0986\u09B0\u09CB \u0989\u09A8\
  \u09CD\u09A8\u09A4 \u099A\u09BE\u09B9\u09BF\u09A6\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF, `nimble_csv` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u098F\
  \u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u098F\u09AC\u0982 \u09B8\u09CB\
  \u099C\u09BE \u09AA\u099B\u09A8\u09CD\u09A6\u0964\n\nElixir-\u098F\u09B0 \u09AC\u09BF\
  \u09B2\u09CD\u099F-\u0987\u09A8 \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0986\u09AA\u09A8\u09BF \u098F\u0995\
  \u099F\u09BF CSV \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09A4\u09C7 \u098F\u09AC\
  \u0982 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7\u09A8."
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কিভাবে:
Elixir তার শক্তিশালী প্যাটার্ন ম্যাচিং এবং পাইপলাইনিং সমর্থনের মাধ্যমে, বাহ্যিক থার্ড-পার্টি লাইব্রেরি ছাড়াই CSV ফাইল দক্ষভাবে হ্যান্ডেল করতে পারে। তবে, আরো উন্নত চাহিদার জন্য, `nimble_csv` লাইব্রেরি একটি দ্রুত এবং সোজা পছন্দ।

### বাহ্যিক লাইব্রেরি ছাড়াই একটি CSV ফাইল পড়া
Elixir-এর বিল্ট-ইন ফাংশন ব্যবহার করে আপনি একটি CSV ফাইল পড়তে এবং পার্স করতে পারেন:

```elixir
defmodule CSVReader do
  def read_file(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# নমুনা ব্যবহার
CSVReader.read_file("data.csv")
# আউটপুট: [["Header1", "Header2"], ["Row1Value1", "Row1Value2"], ["Row2Value1", "Row2Value2"]]
```

### একটি CSV ফাইলে লেখা
অনুরূ

```elixir
defmodule CSVWriter do
  def write_to_file(file_path, data) do
    File.open(file_path, [:write], fn file ->
      Enum.each(data, fn row ->
        IO.write(file, Enum.join(row, ",") <> "\n")
      end)
    end)
  end
end

# নমুনা ব্যবহার
data = [["Header1", "Header2"], ["Value1", "Value2"], ["Value3", "Value4"]]
CSVWriter.write_to_file("output.csv", data)
# output.csv তৈরি করা হবে ডেটা সহ যা CSV হিসেবে ফর্ম্যাটেড হবে
```

### `nimble_csv` ব্যবহার করে
আরো জটিল CSV হ্যান্ডলিং-এর জন্য, `nimble_csv` CSV ডেটা সম্পর্কে কাজ করার জন্য একটি শক্তিশালি এবং নমনীয় উপায় প্রদান করে। প্রথমে, `mix.exs`-এ `nimble_csv` আপনার নির্ভরতাগুলিতে যোগ করুন এবং `mix deps.get` চালান:

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

`nimble_csv` সাহায্যে CSV ডেটা পার্সিং:

```elixir
defmodule MyCSVParser do
  NimbleCSV.define(MyParser, separator: ",", escape: "\\")

  def parse(file_path) do
    file_path
    |> File.stream!()
    |> MyParser.parse_stream()
    |> Enum.to_list()
  end
end

# নমুনা ব্যবহার
CSVReader.read_file("data.csv")
# আউটপুট: [["Header1", "Header2"], ["Row1Value1", "Row1Value2"], ["Row2Value1", "Row2Value2"]]
```

### একটি CSV ফাইলে লেখা
অনুরূ

```elixir
defmodule CSVWriter do
  def write_to_file(file_path, data) do
    File.open(file_path, [:write], fn file ->
      Enum.each(data, fn row ->
        IO.write(file, Enum.join(row, ",") <> "\n")
      end)
    end)
  end
end

# নমুনা ব্যবহার
data = [["Header1", "Header2"], ["Value1", "Value2"], ["Value3", "Value4"]]
CSVWriter.write_to_file("output.csv", data)
# output.csv তৈরি করা হবে ডেটা সহ যা CSV হিসেবে ফর্ম্যাটেড হবে
```

### `nimble_csv` ব্যবহার করে
আরো জটিল CSV হ্যান্ডলিং-এর জন্য, `nimble_csv` CSV ডেটা সম্পর্কে কাজ করার জন্য একটি শক্তিশালি এবং নমনীয় উপায় প্রদান করে। প্রথমে, `mix.exs`-এ `nimble_csv` আপনার নির্ভরতাগুলিতে যোগ করুন এবং `mix deps.get` চালান:

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

`nimble_csv` সাহায্যে CSV ডেটা পার্সিং:

```elixir
defmodule MyCSVParser do
  NimbleCSV.define(MyParser, separator: ",", escape: "\\")

  def parse(file_path) do
    file_path
    |> File.stream!()
    |> MyParser.parse_stream()
    |> Enum.to_list()
  end
end

# নমুনা ব্যবহার
CSVReader.read_file("data.csv")
# আউটপুট: [["Header1", "Header2"], ["Row1Value1", "Row1Value2"], ["Row2Value1", "Row2Value2"]]
```

# আউটপুট `nimble_csv` দিয়ে কাস্টমাইজড হতে পারে আপনি কিভাবে আপনার পার্সার সেটআপ করেছেন তার উপর নির্ভর করে, কিন্�
