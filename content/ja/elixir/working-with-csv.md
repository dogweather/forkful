---
title:                "CSVファイルの操作"
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
CSV（カンマ区切り値）は、表形式のデータをテキストとして保存します。プログラマーは、データ交換やシステム間のインタフェースとして活用します。

## How to: (どうやって？)
ElixirでCSVを扱うには、`CSV`ライブラリが便利です。

```elixir
# CSVライブラリの依存関係を追加
defp deps do
  [{:csv, "~> 2.4"}]
end
```

データを読み込んでみましょう。

```elixir
# CSVデータの読み込み
def read_csv_data(file_path) do
  File.stream!(file_path)
  |> CSV.decode()
  |> Enum.each(fn row ->
    IO.inspect(row)
  end)
end
```

CSVデータの書き込みはこんな感じです。

```elixir
# CSVデータの書き込み
def write_to_csv(file_path, data) do
  CSV.encode(data)
  |> Enum.into(File.stream!(file_path, [:write]))
end
```

サンプル出力:

```elixir
read_csv_data("data.csv")
# 出力:
# ["id", "name", "age"]
# ["1", "Alice", "30"]
# ["2", "Bob", "34"]

write_to_csv("new_data.csv", [["id", "name", "age"], ["3", "Carol", "29"]])
# new_data.csvが作成され、内容が書き込まれる
```

## Deep Dive (深堀り)
CSVは1972年にIBMが開発。JSONやXMLなどの代替フォーマットがあるけれども、シンプルで多くのツールが対応しているため、広く使用されています。Elixirでは、`CSV.decode`でデータを解析し、`CSV.encode`でデータをエンコードします。パフォーマンスを高めるためにストリーム処理を使うことも一般的です。

## See Also (参考情報)
- Elixirの公式ドキュメント: [https://elixir-lang.org/docs.html](https://elixir-lang.org/docs.html)
- CSVライブラリのGitHubページ: [https://github.com/beatrichartz/csv](https://github.com/beatrichartz/csv)
- Elixirのフォーラム: [https://elixirforum.com](https://elixirforum.com)
