---
date: 2024-01-20 17:39:28.423805-07:00
description: "How to: (\u65B9\u6CD5) Python\u3067\u306F\u3001\u6587\u5B57\u5217\u3092\
  \u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\u306E\u306F\u975E\u5E38\u306B\u7C21\
  \u5358\u3067\u3059\u3002`lower()` \u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u3044\u307E\
  \u3059\u3002\u30B5\u30F3\u30D7\u30EB\u30B3\u30FC\u30C9\u3092\u898B\u3066\u307F\u307E\
  \u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.438264-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Python\u3067\u306F\u3001\u6587\u5B57\u5217\u3092\u5C0F\u6587\
  \u5B57\u306B\u5909\u63DB\u3059\u308B\u306E\u306F\u975E\u5E38\u306B\u7C21\u5358\u3067\
  \u3059\u3002`lower()` \u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u3044\u307E\u3059\u3002\
  \u30B5\u30F3\u30D7\u30EB\u30B3\u30FC\u30C9\u3092\u898B\u3066\u307F\u307E\u3057\u3087\
  \u3046\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

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
