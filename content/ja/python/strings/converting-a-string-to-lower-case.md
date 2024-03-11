---
date: 2024-01-20 17:39:28.423805-07:00
description: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u306E\u5168\u3066\u306E\u5927\u6587\u5B57\
  \u3092\u5BFE\u5FDC\u3059\u308B\u5C0F\u6587\u5B57\u306B\u5909\u3048\u308B\u3053\u3068\
  \u3067\u3059\u3002\u30C7\u30FC\u30BF\u306E\u4E00\u8CAB\u6027\u3092\u4FDD\u3064\u305F\
  \u3081\u3084\u3001\u5927\u6587\u5B57\u3068\u5C0F\u6587\u5B57\u3092\u533A\u5225\u3057\
  \u306A\u3044\u691C\u7D22\u3092\u53EF\u80FD\u306B\u3059\u308B\u305F\u3081\u306B\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.105465-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u306E\u5168\u3066\u306E\u5927\u6587\u5B57\
  \u3092\u5BFE\u5FDC\u3059\u308B\u5C0F\u6587\u5B57\u306B\u5909\u3048\u308B\u3053\u3068\
  \u3067\u3059\u3002\u30C7\u30FC\u30BF\u306E\u4E00\u8CAB\u6027\u3092\u4FDD\u3064\u305F\
  \u3081\u3084\u3001\u5927\u6587\u5B57\u3068\u5C0F\u6587\u5B57\u3092\u533A\u5225\u3057\
  \u306A\u3044\u691C\u7D22\u3092\u53EF\u80FD\u306B\u3059\u308B\u305F\u3081\u306B\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
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
