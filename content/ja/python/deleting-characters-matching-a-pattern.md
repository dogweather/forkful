---
title:                "Python: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ
文字のパターンを含む文字を削除することが有用であるかを説明するには1-2文で解説します。

文字を削除することは、大きなテキストデータを処理する際に特に便利です。例えば、特定の単語や文句を含む文章を除外したい場合、文字のパターンを指定して削除することで簡単に行うことができます。

## ハウツー
文字のパターンにマッチする文字を削除するには、Pythonの組み込み関数である`re.sub()`を使用します。

例えば、以下のようなリストがあるとします。

```
items = ["Apple", "Banana", "Cherry"]
```

このリストから`a`が含まれる文字を削除するには、`re.sub()`を使用して次のようにコードを書きます。

```
import re

new_items = [re.sub("a", "", item) for item in items]

print(new_items)
```

Output:
```
["pple", "Bnn", "Cherry"]
```

この例では、`"a"`の代わりに`""`（空の文字列）を指定することで、文字が削除されました。さらに、正規表現を使用して複雑なパターンの文字を削除することもできます。

## ディープダイブ
`re.sub()`の詳細な仕様や他の関連する関数については、[Python公式ドキュメント](https://docs.python.org/ja/3/library/re.html)を参照してください。

また、文字の処理を行う際には、文字コードやエンコーディングにも注意する必要があります。詳しくは[こちらの記事](https://realpython.com/python-encodings-guide/#what-is-a-string-in-python)を参考にしてください。

## See Also
- [Python公式ドキュメント](https://docs.python.org/ja/3/library/re.html)
- [正規表現チュートリアル](https://docs.python.org/ja/3/howto/regex.html)
- [文字コードとエンコーディング](https://realpython.com/python-encodings-guide/)