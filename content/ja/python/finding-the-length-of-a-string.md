---
title:    "Python: 「文字列の長さを見つける」"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Pythonで文字列の長さを見つけるのはなぜ必要なのでしょうか？Pythonで文字列を扱う際に、その長さを把握することは非常に重要です。文字列の長さを知ることで、プログラムを効率的に作成し、問題を解決することができます。

## なぜ？

文字列の長さを知ることは、Pythonでプログラムを作成する際に非常に重要です。文字列の長さを把握することで、文字列を操作する際に必要な分だけ処理を行うことができます。例えば、特定の文字数を超えた場合に文字列をクリップするなど、文字列の長さを知ることでプログラムをより効率的に作成できます。

## 使い方

文字列の長さを見つけるには、Pythonの`len()`関数を使います。例えば、次のようなコードを実行することで、文字列`Hello World!`の長さである12が出力されます。

```Python
print(len("Hello World!"))
```
Output:
```
12
```

Pythonでは、文字列以外にもリストやタプルなどのデータ型に対しても`len()`関数を使用することができます。また、`len()`関数は組み込み関数のため、追加のモジュールをインポートする必要はありません。

## 詳しい解説

文字列の長さを見つけるには、Pythonがどのように文字列を扱うかを理解することが重要です。Pythonでは、全ての文字列をリストのように扱います。そのため、文字列の長さを把握するには、文字列内の文字の数をカウントすることができれば良いことになります。`len()`関数は、そのようなカウントを行ってくれます。

## 併せて読みたい

- [Python で文字列を扱う際の基本的な操作方法](https://www.python.jp/pages/basic/string.html)
- [Pythonの組み込み関数 - `len()`](https://docs.python.org/ja/3/library/functions.html#len)