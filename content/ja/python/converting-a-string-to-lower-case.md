---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を小文字に変換するとは、文字列内のすべての大文字を小文字に変換するプロセスを指します。これは一般に、ユーザー入力データの一貫性を確保し、比較の際のケースセンシティブなエラーを回避するためにプログラマーによって行われます。

## 実装方法：

Pythonでは、文字列を小文字に変換するための組み込み関数`lower()`があります。以下はその使い方です:

```Python
original_string = "Hello, World!"
lowercase_string = original_string.lower()

print(lowercase_string)
```

その結果、以下の出力が表示されます:

```Python
"hello, world!"
```

## より深く見てみましょう:

`lower()`関数の起源はPython言語の初期に遡ります。これは必要な機能の一つで、プログラマーが単純にデータを正規化するのに役立つと認識されていました。

代替手段として、PythonにはUnicodeに対応した大文字から小文字への変換関数`casefold()`もあります。しかし、これは主にヨーロッパの言語で特殊なケースを扱うためのもので、一般的には`lower()`関数を使用する方が良いでしょう。

```Python
original_string = "Der Fluß"
lowercase_string = original_string.casefold()

print(lowercase_string)
```

出力:

```Python
"der fluss"
```

`lower()`関数の内部では、PythonはUnicodeの文字変換プロパティを利用して大文字から小文字への変換を行います。

## 関連リンク：

1. [Python official documentation: String methods](https://docs.python.org/ja/3/library/stdtypes.html#string-methods)
2. [Python: lower() vs casefold()](https://www.journaldev.com/23598/python-string-lower-casefold)