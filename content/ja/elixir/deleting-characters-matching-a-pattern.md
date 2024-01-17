---
title:                "パターンに一致する文字を削除"
html_title:           "Elixir: パターンに一致する文字を削除"
simple_title:         "パターンに一致する文字を削除"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 何となぜ？

パターンに一致する文字を削除するとは、文字列から特定のパターンを持つ文字を削除することを指します。プログラマーは、データの整形やトークンの作成など、文字列操作をする際にしばしばこの操作を行います。

# 方法：

```Elixir
String.replace("Hello World!", ~r/[aeiou]/, "")
```
```
Hll Wrld!
```
上記のコード例では、文字列 "Hello World!" から母音を削除しています。文字列操作は、`String` モジュールの `replace` 関数を使用することで簡単に行うことができます。`~r` の後にパターンを指定し、その後ろに変換後の文字列を指定します。

# 深堀り：

(1) 削除操作のルーツは、正規表現という技術にあります。正規表現は、文字列パターンを記述するための仕組みであり、プログラミング言語やテキストエディタなどで広く使用されています。(2) 一致する文字を置換する代わりに、文字列から一致する文字を抽出することもできます。これには `String.split/3` 関数を使用することができます。(3) 実際には、文字の削除ではなく、新しい文字列を生成することで削除を行っています。

# 参考資料：

この記事では、パターンに一致する文字の削除について紹介しましたが、他にも様々な文字列操作をする方法があります。詳細については、Elixir 公式ドキュメントやコミュニティのフォーラムなどを参照してみてください。

- [Elixir 公式ドキュメント](https://elixir-lang.org/docs.html)
- [Elixir Forum](https://elixirforum.com/)
- [Elixir School](https://elixirschool.com/ja/lessons/basics/basics/)
- [Elixir JapanのSlackチャンネル](https://elixir-jp.connpass.com/event/215747/)