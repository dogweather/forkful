---
title:                "パターンに一致する文字を削除する"
html_title:           "Python: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何を & なぜ?

文字列の中から特定のパターンに一致する文字を削除することを「パターンマッチングに一致する文字の削除」といいます。プログラマーがこれを行うのは、特定の文字を特定の操作を行うために削除することができるためです。

## 方法:

```
# 文字列からxを削除する例
str = "apple banana cherry"
new_str = str.replace("x", "")
print(new_str)
```

出力：
```
aple baana shey
```

## 深く掘り下げる:

**歴史的な文脈:** パターンマッチングに一致する文字の削除は、コンピューターのパターンマッチングアルゴリズムの発展とともに一般的になりました。

**他の選択肢:** 上記の例では、文字列から特定の文字を削除する方法として`replace()`メソッドが使用されましたが、正規表現やループを使用することもできます。

**実装の詳細:** パターンマッチングに一致する文字の削除は、文字列の扱いに詳しいプログラマーにとっては簡単な作業ですが、実際には内部的にはループや文字列操作を行っています。

## 関連リンク:

- [Pythonのreplace()メソッドについて](https://docs.python.org/ja/3/library/stdtypes.html#str.replace)
- [正規表現についてのPythonドキュメント](https://docs.python.org/ja/3.9/howto/regex.html)