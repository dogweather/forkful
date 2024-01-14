---
title:    "Python: デバッグ出力のプリント"
keywords: ["Python"]
---

{{< edit_this_page >}}

## なぜ

デバッグ出力を印刷することによって、コードをより深く理解し、問題を特定することができます。

## 方法

デバッグ出力を印刷するには、Pythonの組み込み関数である`print()`を使用します。以下は、例です。

```Python
x = 5
y = 10
print("xの値は:", x)
print("yの値は:", y)
```

このコードを実行すると、以下のような出力が得られます。

```
xの値は: 5
yの値は: 10
```

## 詳しく掘り下げる

デバッグ出力をプリントすることによって、プログラムの実行中に変数の値を確認することができます。これは、コードのどこで問題が発生しているかを特定するのに役立ちます。また、デバッグ出力を使用して、条件分岐の結果やループの反復回数を確認することもできます。

## 他に見る

- [Pythonの組み込み関数`print()`のドキュメント](https://docs.python.org/ja/3/library/functions.html#print)
- [Pythonのデバッグガイドライン](https://wiki.python.org/ja/moin/PythonDebuggingTips)