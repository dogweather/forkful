---
title:    "Python: 文字列の長さを見つける"
keywords: ["Python"]
---

{{< edit_this_page >}}

## なぜ

文字列の長さを知ることは、プログラミングのとても基本的なスキルです。文字列を扱う上で非常に便利なため、ほとんどのプログラムで必要になります。

## 方法

文字列の長さを取得するには、Pythonの組み込み関数である`len()`を使用します。以下に具体的な例を示します。

```Python
string = "こんにちは、世界!"
print(len(string)) 

# Output:
# 8
```

このコードでは、`len()`関数を使用して文字列を渡し、その長さをプリントすることで、"こんにちは、世界!"という文字列の長さが8であることが分かります。

## 詳細を掘り下げる

`len()`関数は、文字列の他にもリストやタプルなどの様々なデータ型にも使用することができます。また、`len()`は内部的には`__len__()`メソッドを呼び出すことで長さを取得しています。

また、文字列の長さを取得するだけでなく、文字列の特定の文字の数や文字列内に特定の文字が含まれているかどうかを調べることも`len()`関数を使用して行うことができます。

## 参考

- [Python Documentation: Built-in Functions - len()](https://docs.python.org/ja/3/library/functions.html#len)
- [RealPython: Strings and .format()](https://realpython.com/python-strings/)
- [Programiz: Python len()](https://www.programiz.com/python-programming/methods/built-in/len)