---
date: 2024-01-26 04:45:05.245915-07:00
description: "\u4F7F\u3044\u65B9\uFF1A Python\u306B\u306F\u8907\u7D20\u6570\u3092\u30B5\
  \u30DD\u30FC\u30C8\u3059\u308B\u305F\u3081\u306E\u7D44\u8FBC\u307F\u306E\u6A5F\u80FD\
  \u304C\u3042\u308A\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u305D\u308C\u3089\
  \u3092\u3069\u306E\u3088\u3046\u306B\u6271\u3046\u304B\u3092\u793A\u3057\u307E\u3059\
  \uFF1A."
lastmod: '2024-03-13T22:44:41.491884-06:00'
model: gpt-4-0125-preview
summary: "Python\u306B\u306F\u8907\u7D20\u6570\u3092\u30B5\u30DD\u30FC\u30C8\u3059\
  \u308B\u305F\u3081\u306E\u7D44\u8FBC\u307F\u306E\u6A5F\u80FD\u304C\u3042\u308A\u307E\
  \u3059\u3002\u3053\u3053\u3067\u306F\u3001\u305D\u308C\u3089\u3092\u3069\u306E\u3088\
  \u3046\u306B\u6271\u3046\u304B\u3092\u793A\u3057\u307E\u3059\uFF1A."
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

## 使い方：
Pythonには複素数をサポートするための組込みの機能があります。ここでは、それらをどのように扱うかを示します：

```Python
# 複素数の生成
z = 4 + 5j
print(z)  # 出力: (4+5j)

# 実部と虚部へのアクセス
print(z.real)  # 出力: 4.0
print(z.imag)  # 出力: 5.0

# 複素数の算術
w = 1 - 2j
print(z + w)  # 出力: (5+3j)
print(z - w)  # 出力: (3+7j)
print(z * w)  # 出力: (14+2j)
print(z / w)  # 出力: (-3.6+1.2j)

# 絶対値（モジュラス）
print(abs(z))  # 出力: 6.4031242374328485

# 複素数の共役
print(z.conjugate())  # 出力: (4-5j)
```

## ディープダイブ
複素数は、16世紀にジェロラモ・カルダノによって初めて概念化されました。Pythonをはじめとする他のプログラミング言語では、複素数を第一級の市民として扱います。これは、それらが言語に組み込まれており、基本操作のために外部ライブラリをインポートする必要がない、簡単に使える特徴を持っていることを意味します。

しかし、重い数値計算には、`cmath`というライブラリがPythonにはあります。これは複素数専用で、`exp`、`log`、三角関数の操作などの追加機能があります。

Pythonだけでは不十分な場合、特に複素数を含む配列操作には、NumPyを使うことがあります。NumPyは、数値計算のパフォーマンスに欠かせない最適化されたベクトル演算を提供します。

## 参照
もっと学びたい方は、これらのリソースをチェックしてください：

- 複素数に関するPythonの公式ドキュメント: https://docs.python.org/3/library/stdtypes.html#typesnumeric
- `cmath`モジュールのドキュメント: https://docs.python.org/3/library/cmath.html
- 複素数の配列を扱うNumPy: https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics
