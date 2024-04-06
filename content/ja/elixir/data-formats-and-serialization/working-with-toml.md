---
date: 2024-01-26 04:21:06.368038-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A \u307E\u305A\u3001mix\u4F9D\u5B58\
  \u95A2\u4FC2\u306BTOML\u30D1\u30FC\u30B5\u3092\u8FFD\u52A0\u3057\u307E\u3059\u3002\
  \u3053\u306E\u4F8B\u3067\u306F `toml-elixir` \u3092\u4F7F\u7528\u3057\u307E\u3059\
  \uFF1A."
lastmod: '2024-04-05T22:37:49.973006-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u3046\u3084\u3063\u3066\uFF1A \u307E\u305A\u3001mix\u4F9D\u5B58\u95A2\
  \u4FC2\u306BTOML\u30D1\u30FC\u30B5\u3092\u8FFD\u52A0\u3057\u307E\u3059\u3002\u3053\
  \u306E\u4F8B\u3067\u306F `toml-elixir` \u3092\u4F7F\u7528\u3057\u307E\u3059\uFF1A\
  ."
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

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
