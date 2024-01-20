---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何となぜ？

テキストの検索と置換は、特定の文字列を見つけて新しい文字列に置き換える操作を指します。プログラマーはこれを行うことで、重複を避けたり、また情報の更新が必要な場合でも、効率よく作業を進行させることができます。

## どうやるの？

Elixirでは、`String.replace/3`関数を使用してテキストの検索と置換を行います。この関数は、検索用のパターン、置換用の新しい文字列、そして元の文字列を引数として取ります。

以下に簡単な例を挙げます：

```Elixir
IO.puts String.replace("Hello, World!", "World", "Japan")
```

このコードの出力結果は`Hello, Japan!`になります。

## 深い情報

Elixirにおけるテキストの検索と置換は、Erlangという元となる言語から引き継いだ機能で、正規表現もサポートしています。適切な置換パターンを作成することで、さらに高度な文字列操作が可能になります。

また、Elixir以外の言語では、さまざまな名前のメソッドや関数で同じ目的の操作が提供されています。たとえばJavaScriptでは`replace()`、Pythonでは`replace()`、Javaでは`replaceAll()`がそれに対応します。

`String.replace/3`関数は、元の文字列を操作するのではなく、新しい文字列を生成して返すという点についても、考慮する必要があります。これは、Elixirが不変性を重視する言語であるためです。

## 関連リンク

- Elixir公式ドキュメンテーション: `String.replace/3` - https://hexdocs.pm/elixir/String.html#replace/3
- Erlang公式ドキュメンテーション: 文字列操作 - http://erlang.org/doc/man/string.html
- 正規表現についての詳細 - https://ja.wikipedia.org/wiki/正規表現
- プログラミング言語別のテキスト置換ガイド - https://www.tutorialspoint.com/How-to-replace-a-character-in-a-string-in-python