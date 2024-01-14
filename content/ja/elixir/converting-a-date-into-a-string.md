---
title:    "Elixir: 日付を文字列に変換する"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

記事の目的: この記事では、Elixirで日付を文字列に変換する方法について説明します。日付を文字列に変換する理由については、ほとんどのプログラマーにとっては非常に基本的な操作ですが、Elixirの強力な機能を活用するために必要なスキルです。

方法: まずは、"DateTime.to_string/2"というElixirの関数を使用して、日付を文字列に変換する方法をご紹介します。

```Elixir
iex> DateTime.to_string({{2020, 12, 25}, {23, 59, 59}}, "yyyy/MM/dd HH:mm:ss")
"2020/12/25 23:59:59"
```

このように、日付を表すタプルとフォーマット文字列を引数として渡すことで、指定した形式の文字列が返されます。詳細なフォーマットの指定方法については、Elixirの公式ドキュメントを参照してください。

Deep Dive: では、より詳細に日付を文字列に変換する方法について見ていきましょう。まず、"DateTime"モジュールにはさまざまな便利な関数が用意されており、例えば、"DateTime.from_naive"を使用することで、タイムゾーンを考慮せずに日付を表すタプルを作成することができます。

また、Elixirでは、日付を自由に操作することができる "NaiveDateTime" という型も用意されています。この型には、"NaiveDateTime.to_string/2"という関数があり、文字列に変換する場合に便利です。

See Also（参考）:

- Elixir 公式ドキュメント: https://hexdocs.pm/elixir/DateTime.html
- Elixir School: https://elixirschool.com/ja/lessons/basics/dates/

この記事が、Elixirで日付を文字列に変換する方法を簡単に理解するのに役立てば嬉しいです。Elixirを少しでも深く理解するために、ぜひ以上の参考リンクも活用してください。