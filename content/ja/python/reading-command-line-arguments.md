---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？
コマンドライン引数を読むとは、あなたのプログラムが実行時に受け取る追加のパラメータ、つまり引数のことです。プログラマは、プログラムの振る舞いを柔軟に調整したり、外部データを取り込むためにこれらを使用します。

## ハウツー:
Pythonでは、sysモジュールのargv関数を使用してコマンドライン引数にアクセスします。下記に例を挙げます。

```Python
import sys

# コマンドライン引数を表示
print(sys.argv)
```

このコードを`arguments.py`として保存し、コマンドラインから次のように実行します。

```
$ python arguments.py arg1 arg2 arg3
```

出力は次のようになります：

```
['arguments.py', 'arg1', 'arg2', 'arg3']
```

argv[0]はスクリプト名です。コマンドライン引数はargv[1]から始まります。

## ディープダイブ：
コマンドライン引数の概念は、プログラミングの古くから存在するもので、多くのプログラミング言語で利用されています。

Pythonにはargparseなどのより強力なコマンドライン引数パーサーがありますが、sys.argvはその単純さと直感的な性格から広く使用されています。

sys.argvはPythonの組み込みモジュールであり、Python インタープリタへの引数を格納します。これらの引数はスクリプトを起動する時にシェルから渡されます。

## 関連情報：
1. `sys` モジュールの詳細情報については、公式Pythonドキュメンテーションをご覧ください:[Python Docs - sys](https://docs.python.org/ja/3/library/sys.html)
2. さらに高度なパーザー、 `argparse` について学びたい場合は:[Python Docs - argparse](https://docs.python.org/ja/3/library/argparse.html)
3. コマンドライン引数の歴史と理論的背景については:[Wikipedia - Command-line argument](https://en.wikipedia.org/wiki/Command-line_argument)