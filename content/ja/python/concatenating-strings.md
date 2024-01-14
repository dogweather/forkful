---
title:                "Python: 文字列の結合"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ
ストリングを連結することの重要性について説明します。

## 方法
Pythonでストリングを連結する方法は、単純なものから複雑なものまでさまざまです。まずは、``future.insert("こんにちは", "私の名前は")``のように、``insert()``メソッドを使って一つの文字列を別の文字列の中に挿入する方法を見てみましょう。

```Python
name = "山田太郎"
greeting = "こんにちは、" + name + "さん！"
print(greeting)
```

上記のコードを実行すると、次のような出力が得られます。

```
こんにちは、山田太郎さん！
```

次に、``join()``メソッドを使った複数の文字列を連結する方法を見てみましょう。例として、``Japan``、``is``、``beautiful``という3つの文字列を「``-``」で連結してみます。

```Python
words = ["Japan", "is", "beautiful"]
sentence = "-".join(words)
print(sentence)
```

出力は以下の通りになります。

```
Japan-is-beautiful
```

## ディープダイブ
Pythonで文字列を連結する際に注意すべき点は、文字列の特性を理解することです。文字列はイミュータブル（変更不可）なので、新しい文字列が作成されるたびにメモリが消費されてしまいます。そのため、大量の文字列を連結する場合は、``join()``メソッドを使うなどの工夫が必要です。

また、Pythonのスライス（``[start:end]``）を使うと、特定の範囲の文字列を取得することができます。これを利用して、部分的に連結することもできます。

## さらに見る
こちらのリンクを参考にして、さらにPythonで文字列を連結する方法を学んでみましょう。

- https://docs.python.org/ja/3/library/stdtypes.html#text-sequence-type-str
- https://www.geeksforgeeks.org/python-string-concatenation/
- https://www.tutorialspoint.com/python/python_strings.htm