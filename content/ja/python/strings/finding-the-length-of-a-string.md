---
aliases:
- /ja/python/finding-the-length-of-a-string/
date: 2024-01-20 17:48:11.692840-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u8ABF\u3079\u308B\u3053\u3068\
  \u306F\u3001\u305D\u306E\u6587\u5B57\u5217\u304C\u6301\u3064\u6587\u5B57\u306E\u6570\
  \u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u52B9\u7387\u7684\u306A\u30C7\
  \u30FC\u30BF\u51E6\u7406\u306E\u305F\u3081\u306B\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u983B\u7E41\u306B\u30C1\u30A7\
  \u30C3\u30AF\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.555489
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u8ABF\u3079\u308B\u3053\u3068\
  \u306F\u3001\u305D\u306E\u6587\u5B57\u5217\u304C\u6301\u3064\u6587\u5B57\u306E\u6570\
  \u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u52B9\u7387\u7684\u306A\u30C7\
  \u30FC\u30BF\u51E6\u7406\u306E\u305F\u3081\u306B\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u983B\u7E41\u306B\u30C1\u30A7\
  \u30C3\u30AF\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の長さを調べることは、その文字列が持つ文字の数を数えることです。効率的なデータ処理のために、プログラマーは文字列の長さを頻繁にチェックします。

## How to: (方法)
Pythonで文字列の長さを見つけるには、`len()`関数を使います。

```Python
# 文字列の長さを取得
greeting = "こんにちは"
length = len(greeting)
print(length)  # 出力は文字数を示します
```

出力:
```
5
```

## Deep Dive (深掘り)
`len()`関数は、Pythonが最初にリリースされた1990年代から存在しています。この関数は多くのデータ型に対応しており、文字列だけでなくリストやタプルの要素数も返します。`len()`よりもランタイムが長くなることがあります。

文字列の長さを判断する他の方法として、`for`ループを使って一文字ずつ数える方法がありますが、これは効率が悪いため推奨されません。

```Python
greeting = "こんにちは"
count = 0
for character in greeting:
    count += 1
print(count)
```

しかし、PythonのC実装では、文字列オブジェクトはその長さを内部のメモリーに持っており、`len()`関数はこの情報を直接アクセスして取得するため、O(1)の操作となります。

## See Also (関連情報)
- Pythonの公式ドキュメンテーションにある組み込み関数 `len()`: https://docs.python.org/3/library/functions.html#len
- Pythonの文字列操作に関するチュートリアル: https://docs.python.org/3/tutorial/introduction.html#strings
- Unicode文字列としての文字数をカウントする方法についての解説: https://realpython.com/python-encodings-guide/
