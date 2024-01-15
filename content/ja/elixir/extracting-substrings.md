---
title:                "文字列の抽出"
html_title:           "Elixir: 文字列の抽出"
simple_title:         "文字列の抽出"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

文字列から部分文字列を取り出すことについて、あなたは疑問に思ったことはありませんか？それであれば、この記事を読んでください。部分文字列は、プログラミングにおいて非常に重要な役割を果たします。それを取り出す方法を学ぶことは、より効率的なコードを書くための重要なスキルです。

## 方法

部分文字列を取り出すためには、以下のように ```Elixir String``` モジュールを使用します。

```Elixir
"Hello Elixir".slice(2..5)
```
実行すると、次の出力が得られます。

```
"llo "
```
部分文字列の先頭を取り出したい場合は、 ```String.first``` 、末尾を取り出したい場合は ```String.last``` メソッドを使用します。また、```String.slice``` メソッドを使用して、末尾以外の文字列を取り出すこともできます。

## 深堀り

部分文字列の取り出しについてさらに掘り下げると、多くのオプションや機能があることがわかります。例えば、```String.split```を使用すれば、特定の文字を基準に文字列を分割することもできます。また、正規表現を使用することでさらに柔軟に文字列を取り出すことも可能です。

## 関連リンク

- [Elixir Stringモジュールドキュメント](https://hexdocs.pm/elixir/String.html)
- [Elixir文字列操作チュートリアル](https://elixirschool.com/ja/lessons/basics/string-operations/)
- [正規表現の基礎](https://programminghistorian.org/ja/lessons/regexp)