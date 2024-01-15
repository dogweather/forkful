---
title:                "yamlを使ったプログラミング"
html_title:           "Elixir: yamlを使ったプログラミング"
simple_title:         "yamlを使ったプログラミング"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

ElixirでYAMLを扱うのはなぜ？

YAMLは、データ形式を扱うための一般的なツールです。Elixirでは、YAMLを使用してデータをより簡単に読み書きできます。

## 使い方

YAMLを読み取るには、まずexyamlパッケージをインストールする必要があります。次に、YAMLファイルをロードし、データをマップとして取得します。例えば、以下のようなコードを書くことができます。

```Elixir
# exyamlパッケージをインストール
mix deps.get exyaml

# YAMLファイルのロード
yaml = ExYAML.load_file("data.yml")

# マップとして取得
data = yaml.data

# マップの表示
IO.inspect(data)
```

上記のコードを実行すると、YAMLファイルのデータがマップとして表示されます。その後、必要に応じてデータを編集したり、マップを操作したりすることができます。

## ディープダイブ

YAMLでは、データを階層構造で表現することができます。これは、コードをよりシンプルにするために便利です。しかし、階層の深いデータを操作する際には、少し注意が必要です。また、YAMLでは、複数のデータ型を混在させることも可能です。

See Also

- exyamlパッケージのドキュメント(https://hexdocs.pm/exyaml/)
- YAMLの公式サイト(https://yaml.org/)