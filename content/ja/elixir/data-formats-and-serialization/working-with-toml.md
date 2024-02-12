---
title:                "TOMLを扱う方法"
aliases:
- /ja/elixir/working-with-toml/
date:                  2024-01-26T04:21:06.368038-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/working-with-toml.md"
---

{{< edit_this_page >}}

## 何となぜ？
TOMLを使うということは、Elixirを使ってTOML（Tomのオビビアス、ミニマル言語）データを解析し、生成することを意味します。プログラマーは、TOMLが読みやすく、解析がしやすく、ハッシュデータ構造によくマップするため、設定ファイルを扱うのにこれを使用します。

## どうやって：
まず、mix依存関係にTOMLパーサを追加します。この例では `toml-elixir` を使用します：

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

TOMLファイルを読み込む：

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

ElixirのデータをTOMLに変換する：

```elixir
data = %{title: "TOML Example", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

サンプル出力：

```elixir
"title = \"TOML Example\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## 詳細
TOMLは、設定ファイルでの使用を目的として、GitHubの共同設立者であるTom Preston-Wernerによって作成されました。これは、XMLよりも直接的で、YAMLよりも簡潔であることを目指して設計されており、一貫性を維持しています。

代替手段には、人間の可読性とデータ構造の互換性において、それぞれトレードオフがあるJSON、YAML、INIファイルが含まれます。TOMLは、表形式のデータとデータの入れ子のグループ化を明確に表現することに優れています。

Elixirでは、TOMLの扱いは、TOML文字列をElixirのマップへと変換する（またその逆を行う）デコードおよびエンコードライブラリに依存しています。解析は、TOMLの構文ルールに一致させ、それをElixirのデータ型に変換することによって機能します。エンコーディングはその逆で、Elixirのデータ型を有効なTOML構文にマッピングします。

## 参照
- TOML Language: https://toml.io/en/
- `toml-elixir` GitHubリポジトリ: https://github.com/bitwalker/toml-elixir
- `toml-elixir`のHexパッケージ詳細: https://hex.pm/packages/toml_elixir
