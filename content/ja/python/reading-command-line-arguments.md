---
title:                "コンピュータープログラミングの記事のタイトル：「コマンドライン引数の読み込み」"
html_title:           "Python: コンピュータープログラミングの記事のタイトル：「コマンドライン引数の読み込み」"
simple_title:         "コンピュータープログラミングの記事のタイトル：「コマンドライン引数の読み込み」"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読み取ることの重要性について、簡潔に説明します。

## 方法

Pythonでコマンドライン引数を読み取るための簡単な手順を紹介します。

```
```Python
import sys
print(sys.argv)
```

このコードを実行すると、ターミナルで実行した際のコマンドライン引数が出力されます。例えば、`python myscript.py argument1 argument2`というコードを実行すると、`['myscript.py', 'argument1', 'argument2']`というリストが出力されます。

## ディープダイブ

コマンドライン引数を読み取る際によく使われるarsvモジュールについて細かく解説します。このモジュールを使用することで、コマンドライン引数をより柔軟に取得することができます。

```
```Python
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--name", help="Enter your name")
parser.add_argument("--age", help="Enter your age")
args = parser.parse_args()
print(args.name)
print(args.age)
```

このコードでは、引数として`--name`や`--age`を受け取ることができ、それぞれの値を取得することができます。例えば、`python myscript.py --name John --age 25`というコマンドを実行すると、`John`と`25`という出力が得られます。

## 参考リンク
- [公式Pythonドキュメント - コマンドライン引数](https://docs.python.org/ja/3/library/sys.html#sys.arv)
- [Pythonプログラミング入門 サンプル - コマンドライン引数(arsvモジュール)](https://ats-master.github.io/python-lecture01/020.html#t6LN)