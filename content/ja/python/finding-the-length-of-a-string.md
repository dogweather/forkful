---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の長さを取得するとは、その文字列に含まれる文字の数を数える事を指します。文字列の長さは、既定の要素数、文字列の分割、及びデータを適切に処理するために必要な場合、プログラマーによって計算されます。

## どうやって：

Pythonでの文字列長の取得は非常に簡単です。組込みの `len()` 関数を使用します。

```python
s = "こんにちは、Python"
print(len(s))
```
上記のコードの出力は以下のようになります。

```Python
9
```
つまり、文字列 "こんにちは、Python" の長さは9文字であることを意味します。

## 詳細:

歴史的な文脈では、多くの古い言語では文字列長の取得に専用の関数が必要でした。しかし、Pythonでは組み込みの `len()` 関数を提供しています。

また、Pythonでは配列、リスト、タプルなどの他の構造の要素数を知るのにも 'len()'関数が使用できますが、文字列の場合は、文字の数を把握するために使用します。

内部的には、`len()` 関数はPythonオブジェクトの `__len__` メソッドを呼び出します。したがって、 `__len__` メソッドを自分で定義することで、自分のカスタムPythonオブジェクトのための `len()` 関数を実装することも可能です。

## 参照元:

- Python公式ドキュメンテーションにある組み込み関数 `len()`: https://docs.python.org/3/library/functions.html#len 
- Python公式ドキュメンテーションにある特殊メソッド `__len__`: https://docs.python.org/3/reference/datamodel.html#object.__len__