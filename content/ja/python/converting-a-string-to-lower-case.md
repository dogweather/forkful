---
title:                "Python: 「文字列を小文字に変換する」"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

文字列を小文字に変換する理由は、大文字と小文字の区別なく文字列を処理する必要がある場合や、比較する際に一貫性を保つためです。

## How To

文字列を小文字に変換する方法を紹介します。まずは、`lower()`メソッドを使用します。

```Python
# 文字列の宣言
string = "HELLO WORLD"

# lower()メソッドを使用して小文字に変換
new_string = string.lower()

# 変換後の文字列を出力
print(new_string)

# Output: hello world
```

また、`str.casefold()`メソッドも使用することができます。これは、より厳密な小文字変換を行います。例えば、ドイツ語のような文字列では、`lower()`メソッドではうまく変換できない場合があります。そのような場合には、`casefold()`メソッドを使用しましょう。

```Python
# ドイツ語の文字列の例
string = "ßIG"

# casefold()メソッドを使用して小文字に変換
new_string = string.casefold()

# 変換後の文字列を出力
print(new_string)

# Output: ßig
```

## Deep Dive

文字列を小文字に変換する際には、`lower()`メソッドと`casefold()`メソッドの違いを理解することが重要です。`lower()`メソッドは、ASCII文字のみを変換しますが、`casefold()`メソッドはUnicode文字をすべて変換するため、より柔軟な変換が可能です。

また、`lower()`メソッドや`casefold()`メソッドは、元の文字列を変更するのではなく、新しい文字列を返します。そのため、変換後の文字列を変数に代入する必要があります。

## See Also

他にも有用なPythonの文字列操作については、以下のリンクを参考にしてください。

- [Python String Methods](https://www.w3schools.com/python/python_strings_methods.asp)
- [Practical Tips for Using Python String Methods](https://towardsdatascience.com/practical-tips-for-using-python-string-methods-7db6986b8b0d)
- [Python 3's str Methods: Making the Most of an Essential Type](https://realpython.com/python3-string-methods/)