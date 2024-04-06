---
date: 2024-01-26 04:17:18.933531-07:00
description: "\u65B9\u6CD5\uFF1A \u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u306B\
  `python`\u3068\u5165\u529B\u3057\u3066\u3001Python\u306EREPL\u3092\u3059\u3050\u306B\
  \u59CB\u3081\u307E\u3057\u3087\u3046\u3002\u305D\u3053\u306B\u3044\u308B\u9593\u3001\
  \u5358\u7D14\u306A\u64CD\u4F5C\u3084\u8907\u6570\u884C\u306E\u30B3\u30FC\u30C9\u3092\
  \u8A66\u3057\u3066\u307F\u3066\u304F\u3060\u3055\u3044\uFF1A."
lastmod: '2024-04-05T22:37:49.837841-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A \u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u306B`python`\u3068\
  \u5165\u529B\u3057\u3066\u3001Python\u306EREPL\u3092\u3059\u3050\u306B\u59CB\u3081\
  \u307E\u3057\u3087\u3046\u3002\u305D\u3053\u306B\u3044\u308B\u9593\u3001\u5358\u7D14\
  \u306A\u64CD\u4F5C\u3084\u8907\u6570\u884C\u306E\u30B3\u30FC\u30C9\u3092\u8A66\u3057\
  \u3066\u307F\u3066\u304F\u3060\u3055\u3044\uFF1A."
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
