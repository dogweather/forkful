---
title:                "正規表現を使用する"
html_title:           "Elixir: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何をやっているの？
正規表現を使うことは、プログラマーが文字列のパターンを見つけたり、文字列の操作を行ったりするための便利なツールです。正規表現は、文字列とのマッチングや置換を行ったり、バリデーションを行ったりするのに役立ちます。

プログラマーが正規表現を使う理由は、文字列に対する処理が簡単になることです。文字列内の特定のパターンを検索したり、置換したりするために、少ないコードで済ませることができます。

## 使い方：
正規表現は、Elixir内で構築された構文に従って記述する必要があります。例を見てみましょう。

```Elixir
pattern = ~r/foo(bar)?/
string = "foobar"

Regex.run(pattern, string)
#=> ["foobar"]

```
この例では、文字列 "foobar" に対して、"foo" または "foobar" の文字列を検索しています。 `Regex.run` 関数は、文字列内にマッチした結果を返します。

## 更に深く：
正規表現は 1950 年代から存在しており、文字列処理の重要な部分として開発されました。現在では、様々なプログラミング言語で採用されており、Elixirもその一つです。

正規表現以外に、Elixirでは「パターンマッチング」という構文も利用できます。パターンマッチングは、正規表現よりも柔軟性があり、より強力なパターンのマッチングを行えるように設計されています。

正規表現の実装は、Erlangの標準ライブラリである `regexp` モジュールを使用しています。このモジュールは、パフォーマンスが高く、多言語の正規表現実装から影響を受けています。

## 関連情報：
- [ElixirのRegexモジュールドキュメント](https://hexdocs.pm/elixir/Regex.html)
- [正規表現チュートリアル](https://regexone.com/)
- [Elixirのパターンマッチング](https://elixir-lang.org/getting-started/pattern-matching.html)