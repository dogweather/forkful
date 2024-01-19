---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付の解析とは、文字列から日付情報を抽出する操作を指します。一般的には、ユーザー入力や外部源からのデータを処理する際に使用します。

## やり方

以下はElixirで日付の解析を行う具体的なコード例です：

```Elixir
{:ok, datetime, _} = DateTime.from_iso8601("2012-04-01T01:23:10Z")
IO.inspect(datetime)
```

これを実行すると、以下のような結果が得られます：

```Elixir
#DateTime<2012-04-01 01:23:10Z>
```

## ディープダイブ

Elixirのバージョン1.3から、ISO8601の日付と時刻の形式を解析するための機能が追加されました。これはRubyやPythonなど、多くの言語でも実装されています。あるいは、`Timex`のようなライブラリを使用することも選択肢に入ります。

日付解析は固定長の形式を用いるので、計算コストは低く、実装も単純です。一方で、日付の形式が異なる場合、各形式に対応した処置が必要となります。

## 関連資料

- Elixirの公式ドキュメンテーション：[DateTime.from_iso8601](https://hexdocs.pm/elixir/DateTime.html#from_iso8601/1)
- Timexライブラリ：[Timex Docs](https://hexdocs.pm/timex/readme.html)
- ISO8601について：[ISO8601 - Wikipedia](https://ja.wikipedia.org/wiki/ISO_8601)