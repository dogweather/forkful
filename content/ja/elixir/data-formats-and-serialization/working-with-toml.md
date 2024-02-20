---
date: 2024-01-26 04:21:06.368038-07:00
description: "TOML\u3092\u4F7F\u3046\u3068\u3044\u3046\u3053\u3068\u306F\u3001Elixir\u3092\
  \u4F7F\u3063\u3066TOML\uFF08Tom\u306E\u30AA\u30D3\u30D3\u30A2\u30B9\u3001\u30DF\u30CB\
  \u30DE\u30EB\u8A00\u8A9E\uFF09\u30C7\u30FC\u30BF\u3092\u89E3\u6790\u3057\u3001\u751F\
  \u6210\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001TOML\u304C\u8AAD\u307F\u3084\u3059\u304F\u3001\
  \u89E3\u6790\u304C\u3057\u3084\u3059\u304F\u3001\u30CF\u30C3\u30B7\u30E5\u30C7\u30FC\
  \u30BF\u69CB\u9020\u306B\u3088\u304F\u30DE\u30C3\u30D7\u3059\u308B\u305F\u3081\u3001\
  \u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\u306E\u306B\u3053\u308C\u3092\
  \u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:00.917741
model: gpt-4-0125-preview
summary: "TOML\u3092\u4F7F\u3046\u3068\u3044\u3046\u3053\u3068\u306F\u3001Elixir\u3092\
  \u4F7F\u3063\u3066TOML\uFF08Tom\u306E\u30AA\u30D3\u30D3\u30A2\u30B9\u3001\u30DF\u30CB\
  \u30DE\u30EB\u8A00\u8A9E\uFF09\u30C7\u30FC\u30BF\u3092\u89E3\u6790\u3057\u3001\u751F\
  \u6210\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001TOML\u304C\u8AAD\u307F\u3084\u3059\u304F\u3001\
  \u89E3\u6790\u304C\u3057\u3084\u3059\u304F\u3001\u30CF\u30C3\u30B7\u30E5\u30C7\u30FC\
  \u30BF\u69CB\u9020\u306B\u3088\u304F\u30DE\u30C3\u30D7\u3059\u308B\u305F\u3081\u3001\
  \u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\u306E\u306B\u3053\u308C\u3092\
  \u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
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
