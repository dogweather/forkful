---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？ ("What & Why?")

日付を文字列に変換するとは、一般の日付形式をテキスト形式に変更することです。これは、日付の表示の形式を自由に制御したいときや、日付をフォーマット指定で表示する必要があるときなどにプログラマーが行います。

## どうやって ("How to:")

Elixirで日付を文字列に変換するための基本的な方法を見てみましょう:

```Elixir
iex> date = Date.new!(2022, 1, 1)
~D[2022-01-01]
iex> date |> Date.to_iso8601()
"2022-01-01"
```

上記のコードでは、新しい日付を作り、それをISO 8601日付フォーマットの文字列に変換しています。

## ディープダイブ ("Deep Dive")

歴史的には、文字列化された日付は、人間による読解やデバッグ作業を簡略化します。また、異なるシステム間で日付データをやり取りする場合にも頻繁に使用されます。

代替方法として、`Date.to_string/1` も使用可能ですが、こちらの方法ではElixir固有の日付形式（例えば `~D[2022-01-01]`）の文字列に変換します。

```Elixir
iex> date |> Date.to_string()
"~D[2022-01-01]"
```
`Date.to_iso8601/1`関数は、エリクサーの日付/時間型に関する多くの関数と同様に、Elixirの日付/時間ライブラリ内で定義されています。

## 参照 ("See Also")

- Elixirの公式ドキュメンテーション: [Date docs](https://hexdocs.pm/elixir/Date.html)
- ISO 8601日付フォーマットについての詳細: [Wikipedia](https://ja.wikipedia.org/wiki/ISO_8601)