---
title:                "Python: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

Pythonで文字列を連結するのは、複数の文字列を一つの大きな文字列にまとめるためです。

## 方法

```Python
# 文字列の連結
str1 = "Hello"
str2 = "World"
str3 = "!"
sentence = str1 + " " + str2 + str3
print(sentence)

# 出力結果: Hello World!

# 文字列と数字の連結
num1 = 10
num2 = 20
result = str(num1) + " + " + str(num2) + " = " + str(num1 + num2)
print(result)

# 出力結果: 10 + 20 = 30
```

## ディープダイブ

文字列を連結する方法には、`+`演算子や`str.join()`メソッドを使用する方法があります。また、長い文字列を連結する場合は、`str.format()`メソッドを使うとスマートな方法で文字列を組み立てることができます。

### `+`演算子

`+`演算子を使用すると、文字列同士を直接結合することができます。

### `str.join()`メソッド

`str.join()`メソッドは、指定した文字列を区切り文字として、複数の文字列を連結することができます。

### `str.format()`メソッド

`str.format()`メソッドは、指定した変数や式を文字列内に埋め込むことができます。これにより、複雑な文字列を比較的簡単なコードで作成することができます。

## See Also

- [Python 文字列操作](https://docs.python.org/ja/3/library/string.html)
- [Python 文字列フォーマット](https://docs.python.org/ja/3/library/string.html#formatstrings)