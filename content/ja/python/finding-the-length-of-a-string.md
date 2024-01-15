---
title:                "文字列の長さを見つける"
html_title:           "Python: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列の長さを求めることの目的としては、プログラミングにおいて文字列を処理する上で重要な情報を得ることができるからです。例えば、文字列の一部のみを抽出する際にはその長さを知る必要があります。

## 方法
文字列の長さを求める方法は簡単です。Pythonでは、組み込み関数の`len()`を使用します。例えば、以下のコードを実行すると、文字列`Hello World`の長さが求められます。

```Python
length = len("Hello World")
print(length)
```

上記のコードの出力は`11`となります。また、文字列変数に対しても同じように`len()`を使用することができます。

```Python
message = "こんにちは"
length = len(message)
print(length)
```

上記のコードの出力は`5`となります。

## ディープダイブ
Pythonでは、文字列の長さを求める際にどのような処理が行われているのかを知ることができます。文字列は実際には文字のリストとして扱われており、`len()`関数はこのリストの要素数を返しています。そのため、文字列がどのように構成されているかを知ることで、`len()`関数の動きも理解することができます。

## See Also
- [Python 公式ドキュメント](https://docs.python.org/3/library/functions.html#len)
- [Codecademy: Strings](https://www.codecademy.com/learn/learn-python-3/modules/learn-python3-strings)