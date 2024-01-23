---
title:                "文字列の補間"
date:                  2024-01-20T17:52:01.036499-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列補間は、変数や計算結果を直接文字列に埋め込むプロセスです。プログラマはこれを使って、コードをより読みやすく、メンテナンスしやすくするために行います。

## How to: (方法)
```python
# 基本的な文字列補間
name = "世界"
message = f"こんにちは、{name}！"
print(message) # 出力: こんにちは、世界！

# 計算を埋め込む
a, b = 5, 10
print(f"{a} + {b} は {a + b} です。") # 出力: 5 + 10 は 15 です。

# フォーマットを指定
temperature = 30.4444
print(f"温度は {temperature:.2f}度です。") # 出力: 温度は 30.44度です。
```

## Deep Dive (深い潜水)
文字列補間は Python 3.6 の時点で導入された `f-string` によって大きく改善されました。それ以前は `%` オペレーターや `str.format()` メソッドがよく使われていましたが、f-string はこれらよりも読みやすく、速いです。

古いメソッド:
```python
name = "世界"
message = "こんにちは、%s！" % name
print(message) # 出力: こんにちは、世界！

message = "こんにちは、{}！".format(name)
print(message) # 出力: こんにちは、世界！
```

f-string の内部的には、`FORMAT_VALUE` と `BUILD_STRING` という２つの命令で実行されます。文字列の各部分が評価され、最終的な文字列へと結合されるプロセスは Python インタプリタにより効率的に行われます。

## See Also (参照)
- [PEP 498 -- Literal String Interpolation](https://www.python.org/dev/peps/pep-0498/)
- [Python 3.x documentation for f-strings](https://docs.python.org/3/reference/lexical_analysis.html#f-strings)
- [The Python Formatter class](https://docs.python.org/3/library/string.html#string.Formatter)
