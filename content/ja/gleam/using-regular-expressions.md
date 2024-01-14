---
title:                "Gleam: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ使うのか

正規表現を使用する理由はたくさんありますが、主な理由は文字列のパターンを検索したり置換したりするためです。例えば、特定のメールアドレスや電話番号を含む文章を探したり、HTMLタグを削除したりする場合に便利です。

## 使い方

正規表現を使用するには、まず`re`モジュールをインポートする必要があります。次に、`match`関数を使用して指定したパターンに対するテキストのマッチングを実行します。以下の例では、テキストからメールアドレスを抽出する方法を示します。

```Gleam
import re

email_pattern = "[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+"

text = "私のメールアドレスはabc123@example.comです。"

match(email_pattern, text)

```

上記のコードを実行すると、`"abc123@example.com"`というメールアドレスが抽出されます。また、`replace`関数を使用すれば、パターンにマッチした部分を置換することもできます。

## ディープダイブ

正規表現にはさまざまな機能やパターンがあります。例えば、`*`や`+`のようなメタ文字を使用することで、任意の文字数の繰り返しを表現することができます。また、`?`を使用することで、パターンの一部があってもなくてもマッチするようにすることもできます。さらに、グループ化を行うことで、複数のパターンを同時にマッチさせることができるようになります。

正規表現の詳細な使い方やパターンの種類については、[Gleam公式ドキュメント](https://gleam.run/docs/?redirect_to=https%3A%2F%2Fgleam.run%2Fblog%2Fregex%2F)を参考にしてください。

## 併せて参考にしてほしいリンク

- [正規表現チートシート](https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf)
- [正規表現のデバッグ方法](https://rubular.com/)
- [Pythonの正規表現チュートリアル](https://docs.python.org/ja/3/howto/regex.html)

## 参考文献

- [Gleam公式ドキュメント](https://gleam.run/docs/?redirect_to=https%3A%2F%2Fgleam.run%2Fblog%2Fregex%2F)
- [正規表現チートシート](https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf)
- [正規表現のデバッグ方法](https://rubular.com/)
- [Pythonの正規表現チュートリアル](https://docs.python.org/ja/3/howto/regex.html)