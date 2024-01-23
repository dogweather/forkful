---
title:                "YAMLを扱う"
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAMLは設定ファイルやデータのシリアライズに使われる。簡潔で人間が読みやすいため、Elixirの開発でよく利用される。

## How to:
ElixirでYAMLを扱うため、`:yamerl`ライブラリを使います（:yamerlはErlangのライブラリです）。これをmix.exsに追加して、依存関係を取得しましょう。

```elixir
# mix.exsに依存関係を追加
defp deps do
  [
    {:yamerl, "~> 0.8"}
  ]
end
```

YAMLの読み込み例です。

```elixir
# YAMLファイルを読み込む
{:ok, yaml} = File.read("config.yaml")
# YAMLをパースする
{:ok, parsed_yaml} = :yamerl_constr.string(yaml)
# 結果を表示
IO.inspect(parsed_yaml)
```

これにより、`config.yaml`の内容がElixirのデータ構造に変換されます。

## Deep Dive
YAMLは"YAML Ain't Markup Language"の略で、XMLやJSONのようなデータフォーマットの一種です。Elixirは直接的なYAMLサポートはないため、`:yamerl`のような外部のライブラリが必要です。JSONの代わりにYAMLを使う理由は、コメントのサポートや読みやすさにあります。

## See Also
- Elixirの公式ドキュメント: https://elixir-lang.org/docs.html
- `:yamerl`のHexドキュメント: https://hexdocs.pm/yamerl/Yamerl.html
- YAML公式サイト: https://yaml.org
- JSONとYAMLの比較: https://www.json2yaml.com/
