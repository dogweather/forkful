---
title:                "Elixir: パターンに一致する文字を削除する"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

文字列にマッチする文字を削除することが重要である理由は、データ処理や情報取得の際に必要な情報を特定のパターンに基づいて除外するためです。

## 方法

文字列からマッチする文字を削除する方法は簡単です。Elixirでは、`Regex.replace/3`関数を使用することができます。以下は、簡単な例です。

```Elixir
pattern = ~r/[aeiou]/ # 削除するパターンを定義
string = "hello world" # 文字列の定義
Regex.replace(pattern, string, "") # "hello world"から"e"と"o"を削除した結果は"hll wrld"となります
```

## ディープダイブ

`Regex.replace/3`関数は、マッチしたすべての文字を削除しますが、単一の文字だけでなく、パターンに基づいて削除することもできます。以下の例を見てみましょう。

```Elixir
pattern = ~r/[aeiou]/ # 削除するパターンを定義
whole_string = "Lorem ipsum dolor sit amet" # 文字列の定義
pattern_string = "lor" # パターンとして使用する文字列
Regex.replace(pattern, string, pattern_string) # "Lorem ipsum dolor sit amet"から"o"と"e"を削除した結果は"Lrem ipsum drlr sit amet"となります
```

このように、パターンに合致するすべての文字を削除することができます。

## 他の記事を見る

- [Elixir公式ドキュメント](https://elixir-lang.org/getting-started/introduction.html)
- [正規表現の基本](https://qiita.com/kazup01/items/c7c3f04f84446986afa9)
- [Elixirでの文字列操作](http://elixir-users.jp/posts/111)
- [Regexモジュールのドキュメント](https://hexdocs.pm/elixir/Regex.html)