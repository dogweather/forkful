---
title:                "文字列を大文字にする"
html_title:           "Python: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列の最初の文字を大文字にすることを英語で "capitalizing a string" といいます。文字列の最初の文字を大文字にする理由は、一貫性を保ち、視覚的に読みやすくするためです。

## どうするか:

Python で文字列を大文字にする基本的な方法は、組み込み関数の `capitalize()` を使うことです。
```Python
text = "hello, world!"
text = text.capitalize()
print(text)  
```
出力は以下の通りです。
```Python
Hello, world!
```

## ディープダイブ:

`capitalize()` 関数は Python 言語の一部として生まれたときから存在しています。その他に、`title()` という関数もあり、これは全ての単語の最初の文字を大文字にします。
```Python
text = "hello, world!"
text = text.title()
print(text)  
```
出力は以下の通りです。
```Python
Hello, World!
```
Python の内部では、文字列の最初の文字を大文字にするために ASCII 値の変換が行われています。小文字から大文字への変換は ASCII チャートで 32 の差があります。

## 参考資料:

- Python official documentation on string methods: https://docs.python.org/3/library/stdtypes.html#string-methods
- ASCII chart for reference: https://ascii.cl/