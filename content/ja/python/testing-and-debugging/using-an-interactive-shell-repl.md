---
date: 2024-01-26 04:17:18.933531-07:00
description: "REPL\u3001\u307E\u305F\u306FRead-Eval-Print Loop\u306F\u3001\u5358\u4E00\
  \u306E\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u3092\u53D7\u3051\u53D6\u308A\u3001\u305D\
  \u308C\u3092\u5B9F\u884C\u3057\u3001\u7D50\u679C\u3092\u30E6\u30FC\u30B6\u30FC\u306B\
  \u8FD4\u3059\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30AF\u30A4\u30C3\u30AF\u30C6\u30B9\
  \u30C8\u3001\u5B66\u7FD2\u3001\u30C7\u30D0\u30C3\u30B0\u3001\u307E\u305F\u306F\u305D\
  \u306E\u5834\u3067\u306E\u8A08\u7B97\u3092\u884C\u3046\u305F\u3081\u306B\u3053\u308C\
  \u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.501542-06:00'
model: gpt-4-0125-preview
summary: "REPL\u3001\u307E\u305F\u306FRead-Eval-Print Loop\u306F\u3001\u5358\u4E00\
  \u306E\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u3092\u53D7\u3051\u53D6\u308A\u3001\u305D\
  \u308C\u3092\u5B9F\u884C\u3057\u3001\u7D50\u679C\u3092\u30E6\u30FC\u30B6\u30FC\u306B\
  \u8FD4\u3059\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30AF\u30A4\u30C3\u30AF\u30C6\u30B9\
  \u30C8\u3001\u5B66\u7FD2\u3001\u30C7\u30D0\u30C3\u30B0\u3001\u307E\u305F\u306F\u305D\
  \u306E\u5834\u3067\u306E\u8A08\u7B97\u3092\u884C\u3046\u305F\u3081\u306B\u3053\u308C\
  \u3092\u4F7F\u7528\u3057\u307E\u3059\u3002."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

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
