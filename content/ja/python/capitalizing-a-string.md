---
title:    "Python: 文字列の大文字変換"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

**## Why** 
プログラミングを始めたばかりの方にとって、文字列を大文字にする必要性はわかりにくいかもしれません。しかし、正しい文字の大文字と小文字の使い分けは、プログラミングの世界で重要なスキルです。文字列を大文字にすることで、データの整列や操作をより簡単に行うことができるため、積極的に学習する価値があります。

**## How To**
```Python
# 文字列を大文字にする方法
my_string = "hello world"
print(my_string.upper())
```
```Python
# 出力結果
HELLO WORLD
```
簡単なコード例を示しましたが、文字列を大文字にするには`upper()`という組み込み関数を使います。文字列の後に`.upper()`を追加することで、すべての文字が大文字に変換されます。

```Python
# 大文字と小文字が混在している場合の例
my_string = "HeLlO WoRld"
print(my_string.upper())
```
```Python
# 出力結果
HELLO WORLD
```

**## Deep Dive**
文字列を大文字に変換する`upper()`関数は、実際には文字列型の`str`クラスのメソッドとして定義されています。つまり、`文字列.upper()`のように書くことで、文字列のメソッドとして呼び出すことができるのです。また、`upper()`関数は大文字以外の文字にも対応しており、例えば日本語のようなマルチバイト文字も正しく変換することができます。

さらに、`upper()`関数は元の文字列を変更せずに新しい文字列を返すため、元の文字列は変化しません。そのため、必要に応じて新しい変数に結果を保存することができます。

**See Also**
- [Pythonの文字列メソッド](https://docs.python.org/ja/3/library/stdtypes.html#string-methods)
- [大文字変換のチートシート](https://www.11online.us/support-portal/python-cheat-sheet-tutorial-for-python-beginners/cs-strings-in-python-tutorial-for-beginners)

この記事では文字列を大文字にする方法について学びましたが、実際には文字列を操作するためのさまざまな方法があります。ぜひ上記のリンクやPythonのドキュメントを参考にして、さまざまな文字列操作の方法を学んでみてください。