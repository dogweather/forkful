---
title:                "Elixir: 文字列の先頭大文字化"
simple_title:         "文字列の先頭大文字化"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字に変換するのに取り組むことの *なぜ* について説明します。

大文字に変換することは、文字列を整えるための一般的なタスクです。例えば、データベースから取得した文字列を必要な形式に整形する場合や、ユーザーが入力した文字列を処理する場合に大文字に変換することが必要になるでしょう。

## How To

文字列を大文字に変換する方法をコーディング例と ```Elixir ... ``` コードブロックを使って示します。

```Elixir
# 文字列を大文字に変換する
string = "hello world"
IO.puts(String.upcase(string))

# 出力: HELLO WORLD
```

これで、文字列を簡単に大文字に変換できます。

## Deep Dive

文字列を大文字に変換するために使われる関数について詳しく説明します。Elixirでは、```String.upcase/1``` 関数を使って文字列を大文字に変換できます。引数には変換する文字列を渡します。この関数は、標準ライブラリの一部として提供されているため、追加の設定やインストールは必要ありません。

## See Also

より詳しい情報が必要な場合は、次のリンクをご覧ください。

- [Elixir 公式ドキュメント](https://elixir-lang.org/getting-started/basic-types.html#strings)
- [Elixir School の「Strings」のセクション](https://elixirschool.com/ja/lessons/basics/string/)
- [「日本語文字列を大文字・小文字変換」の記事](https://qiita.com/pepeok/collection/a97ae87af96b6600a97e)

以上で、文字列を大文字に変換する方法についての説明を終わります。ありがとうございました！