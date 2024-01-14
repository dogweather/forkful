---
title:                "Python: 文字列の大文字化"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

Pythonで文字列を大文字にすることを行う理由は、データの整理やフォーマット変換のために必要な場合があります。

## 方法

まず、Pythonの`upper()`メソッドを使って文字列を大文字に変換します。例えば、次のように書くことができます。

```Python
test_string = "hello, world"
print(test_string.upper())
```

出力は以下の通りになります。

```
HELLO, WORLD
```

また、`capitalize()`メソッドを使って最初の文字だけを大文字にすることもできます。例えば、次のように書くことができます。

```Python
test_string = "hello, world"
print(test_string.capitalize())
```

出力は以下の通りになります。

```
Hello, world
```

## 深堀り

文字列を大文字にすると、同じ文字列を持つ要素を比較する際に、大文字と小文字を区別しなくなります。また、大文字に変換された文字列は一部のファイルシステムやデータベースでは正しい順序で並べ替えられることがあります。

## 参考リンク

- [Pythonの文字列操作について](https://docs.python.org/ja/3/library/stdtypes.html#text-sequence-type-str)
- [Pythonの文字列メソッドの一覧](https://www.w3schools.com/python/python_ref_string.asp)