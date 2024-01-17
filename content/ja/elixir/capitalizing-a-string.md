---
title:                "文字列を大文字化する"
html_title:           "Elixir: 文字列を大文字化する"
simple_title:         "文字列を大文字化する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何& なぜ?

文字列のキャピタライズとは、文字列の最初の文字を大文字に変換することです。プログラマーは、データを見やすく整理したり、データベースの検索を簡単にするために、文字列をキーワードとして使用する場合があります。

## 方法:

```Elixir
name = "tom"
capitalized = String.capitalize(name)
IO.puts(capitalized) 
```

出力結果:
```Elixir
Tom
```

## 深堀り:

文字列のキャピタライズは、英語の文法的規則に基づいています。文字列の最初の文字を大文字にすることにより、英語の文のように文化に適した書き方ができます。この機能は、Elixir言語だけでなく、多くのプログラミング言語でも利用できます。

## 関連サイト:

- [Elixir 公式ドキュメント] (https://elixir-lang.org/getting-started/string.html#capitalization)
- [String.capitalizeの詳細] (https://hexdocs.pm/elixir/String.html#capitalize/1)