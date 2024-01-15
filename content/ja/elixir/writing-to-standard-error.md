---
title:                "標準エラーへの書き込み"
html_title:           "Elixir: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

標準エラー出力に書き込むことは、プログラムのデバッグやログを取るために非常に有用です。エラー出力を利用することで、プログラムの実行中に発生した問題を確認し、改善することができます。

## 書き込み方

標準エラー出力に書き込むには、Elixirの`IO.write/2`関数を使用します。例えば、以下のように書き込むことができます。

```Elixir
IO.write(:stderr, "エラーメッセージ")
```

これにより、"エラーメッセージ"が標準エラー出力に出力されます。

## 深堀り

標準エラー出力とは、プログラム実行時に発生したエラーや警告、デバッグ情報などを表示するための出力ストリームです。`IO.write/2`関数は、第1引数で指定したストリームに第2引数で与えたデータを書き込むことができます。標準エラー出力はデフォルトでは画面に出力されますが、リダイレクトなどを使用することで、出力先を変更することができます。

## 参考リンク

- [Elixir公式ドキュメント](https://hexdocs.pm/elixir/IO.html#write/2)
- [標準エラー出力を利用したデバッグ方法の解説](https://matsu-blog.netlify.app/posts/20100831_stderror)