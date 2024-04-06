---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:44.213202-07:00
description: "\u65B9\u6CD5\uFF1A Elixir\u306F\u3001\u5F37\u529B\u306A\u30D1\u30BF\u30FC\
  \u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\u3068\u30D1\u30A4\u30D7\u30E9\u30A4\u30F3\u306E\
  \u30B5\u30DD\u30FC\u30C8\u3092\u5099\u3048\u3066\u3044\u308B\u305F\u3081\u3001\u30B5\
  \u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u306A\u3057\
  \u3067\u3082CSV\u30D5\u30A1\u30A4\u30EB\u3092\u52B9\u7387\u7684\u306B\u6271\u3046\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3057\u304B\u3057\u3001\u3088\u308A\
  \u9AD8\u5EA6\u306A\u30CB\u30FC\u30BA\u306B\u5BFE\u3057\u3066\u306F\u3001`nimble_csv`\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u304C\u901F\u304F\u3066\u7C21\u6F54\u306A\u9078\u629E\u80A2\
  \u3067\u3059\u3002"
lastmod: '2024-04-05T22:37:49.971942-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Elixir\u306F\u3001\u5F37\u529B\u306A\u30D1\u30BF\u30FC\
  \u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\u3068\u30D1\u30A4\u30D7\u30E9\u30A4\u30F3\u306E\
  \u30B5\u30DD\u30FC\u30C8\u3092\u5099\u3048\u3066\u3044\u308B\u305F\u3081\u3001\u30B5\
  \u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u306A\u3057\
  \u3067\u3082CSV\u30D5\u30A1\u30A4\u30EB\u3092\u52B9\u7387\u7684\u306B\u6271\u3046\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3057\u304B\u3057\u3001\u3088\u308A\
  \u9AD8\u5EA6\u306A\u30CB\u30FC\u30BA\u306B\u5BFE\u3057\u3066\u306F\u3001`nimble_csv`\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u304C\u901F\u304F\u3066\u7C21\u6F54\u306A\u9078\u629E\u80A2\
  \u3067\u3059\u3002"
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 方法：
Elixirは、強力なパターンマッチングとパイプラインのサポートを備えているため、サードパーティのライブラリなしでもCSVファイルを効率的に扱うことができます。しかし、より高度なニーズに対しては、`nimble_csv`ライブラリが速くて簡潔な選択肢です。

### 外部ライブラリなしでCSVファイルを読む
Elixirの組み込み関数を使用してCSVファイルを読み取り、解析することができます：

```elixir
defmodule CSVReader do
  def read_file(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# サンプル使用法
CSVReader.read_file("data.csv")
# 出力: [["Header1", "Header2"], ["Row1Value1", "Row1Value2"], ["Row2Value1", "Row2Value2"]]
```

### CSVファイルに書き込む
同様に、CSVファイルにデータを書き込むには：

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

# サンプル使用法
data = [["Header1", "Header2"], ["Value1", "Value2"], ["Value3", "Value4"]]
CSVWriter.write_to_file("output.csv", data)
# CSV形式のデータを含むoutput.csvを作成
```

### `nimble_csv`の使用
より複雑なCSVの扱いには、`nimble_csv`はCSVデータを扱うための強力で柔軟な方法を提供します。まず、`nimble_csv`を`mix.exs`の依存関係に追加し、`mix deps.get`を実行します：

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

`nimble_csv`でCSVデータを解析：

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

# サンプル使用法
MyCSVParser.parse("data.csv")
# nimble_csvを使用した出力は、パーサーの設定によりますが、一般的にはリストのリストやタプルのように見えます。
```

`nimble_csv`を使用してCSVデータを書き込むには、データを適切な形式に手動で変換してからファイルに書き込む必要があります。これはプレインなElixirの例と似ていますが、正しくフォーマットされたCSV行を生成するために`nimble_csv`を活用しています。

タスクの複雑さに適したアプローチを選択することで、ElixirでCSVファイルを非常に柔軟かつ強力に扱うことができます。
