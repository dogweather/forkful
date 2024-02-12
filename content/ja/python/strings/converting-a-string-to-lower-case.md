---
title:                "文字列を小文字に変換"
aliases:
- /ja/python/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:28.423805-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列を小文字に変換するとは、テキストの全ての大文字を対応する小文字に変えることです。データの一貫性を保つためや、大文字と小文字を区別しない検索を可能にするためにプログラマーはこれを行います。

## How to: (方法)
Pythonでは、文字列を小文字に変換するのは非常に簡単です。`lower()` メソッドを使います。サンプルコードを見てみましょう。

```python
original_string = "Hello, World!"
lowercase_string = original_string.lower()

print(lowercase_string)
```

出力は次のようになります:

```
hello, world!
```

## Deep Dive (深掘り)
文字列を小文字に変換する処理は、1960年代からのプログラミング言語に存在します。古い言語では個別に対応した関数が必要でしたが、Pythonでは組み込みメソッドを利用します。代替方法として、Unicode データの`casefold()`メソッドがあり、より広範なケースマッピングを提供します。例えば、ドイツ語のエスツェット（ß）は「ss」として小文字化されます。

実装では、Pythonは内部的にUnicodeテーブルを利用していて、文字ごとの対応する小文字を参照します。これは複数の言語や特殊な文字に対応するために重要です。

```python
german_string = "Straße" 
casefolded_string = german_string.casefold()

print(casefolded_string)
```

出力:

```
strasse
```

## See Also (関連情報)
- Python公式ドキュメント: [str.lower()](https://docs.python.org/3/library/stdtypes.html#str.lower)
- Unicode案内: [Case Folding](https://www.unicode.org/reports/tr21/tr21-5.html)
- Python公式ドキュメント: [str.casefold()](https://docs.python.org/3/library/stdtypes.html#str.casefold)
