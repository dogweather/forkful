---
title:                "インタラクティブシェル（REPL）の使用"
aliases:
- ja/python/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:17:18.933531-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 何となぜ？
REPL、またはRead-Eval-Print Loopは、単一のユーザー入力を受け取り、それを実行し、結果をユーザーに返すプログラミング環境です。プログラマーは、クイックテスト、学習、デバッグ、またはその場での計算を行うためにこれを使用します。

## 方法：
コマンドラインに`python`と入力して、PythonのREPLをすぐに始めましょう。そこにいる間、単純な操作や複数行のコードを試してみてください：

```Python
>>> 1 + 1
2
>>> for i in range(3):
...     print(i)
... 
0
1
2
```

関数を試して、即時のフィードバックを得る：

```Python
>>> def greet(name):
...     return "Hello, " + name + "!"
... 
>>> greet("Alice")
'Hello, Alice!'
```

ライブラリを試して、リアルタイムでその機能を探求する：

```Python
>>> import math
>>> math.sqrt(16)
4.0
```

`exit()`または`Ctrl+D`（Windowsでは時々`Ctrl+Z`）で簡単に終了します。

## 深掘り
REPLの概念はPythonに固有のものではなく、Lispの時代からあります。多くの言語が、コードに対する実践的なアプローチのために、この即時、インタラクティブな環境を提供しています。ネイティブPythonシェルの代替としては、IPythonやJupyter Notebookがあり、これらは強化されたインタラクティビティ、より多くの機能、および他のツールとのより良い統合を提供します。Pythonの標準REPLはシンプルですが、Pythonの全能力を組み込んでおり、複雑なオブジェクトやマルチスレッドプログラムを扱いますが、より高度なツールに存在する自動補完や構文強調表示などの機能は欠けています。

## 参照
- [インタプリタに関するPythonの公式文書](https://docs.python.org/3/tutorial/interpreter.html)
- [IPython: 高度なPythonシェル](https://ipython.org/)
- [Jupyterプロジェクト](https://jupyter.org/)
