---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:24:03.388408-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u6587\u5B57\u5217\u30EA\
  \u30C6\u30E9\u30EB\u5185\u306B\u5F0F\u3092\u57CB\u3081\u8FBC\u3080\u65B9\u6CD5\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u52D5\u7684\u306B\u6587\u5B57\
  \u5217\u306B\u5024\u3092\u633F\u5165\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\
  \u4F7F\u7528\u3057\u3001\u3053\u308C\u306B\u3088\u308A\u30B3\u30FC\u30C9\u304C\u5F93\
  \u6765\u306E\u6587\u5B57\u5217\u9023\u7D50\u3088\u308A\u3082\u8AAD\u307F\u3084\u3059\
  \u304F\u3001\u6E05\u6F54\u306B\u306A\u308A\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.481573-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u6587\u5B57\u5217\u30EA\
  \u30C6\u30E9\u30EB\u5185\u306B\u5F0F\u3092\u57CB\u3081\u8FBC\u3080\u65B9\u6CD5\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u52D5\u7684\u306B\u6587\u5B57\
  \u5217\u306B\u5024\u3092\u633F\u5165\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\
  \u4F7F\u7528\u3057\u3001\u3053\u308C\u306B\u3088\u308A\u30B3\u30FC\u30C9\u304C\u5F93\
  \u6765\u306E\u6587\u5B57\u5217\u9023\u7D50\u3088\u308A\u3082\u8AAD\u307F\u3084\u3059\
  \u304F\u3001\u6E05\u6F54\u306B\u306A\u308A\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
---

{{< edit_this_page >}}

## 何となぜ？
文字列補間とは、文字列リテラル内に式を埋め込む方法です。プログラマーは動的に文字列に値を挿入するためにこれを使用し、これによりコードが従来の文字列連結よりも読みやすく、清潔になります。

## 方法：
Python 3.6以上では、f-stringsを使用して文字列を補間できます。以下の方法です：

```Python
name = 'Alice'
age = 30
greeting = f"こんにちは、{name}さん。あなたは{age}歳です。"

print(greeting)
```

出力：
```
こんにちは、Aliceさん。あなたは30歳です。
```

中括弧内に式を使用することもできます：

```Python
a = 5
b = 10
info = f"五加十は{a + b}です、{2 * (a + b)}ではありません。"

print(info)
```

出力：
```
五加十は15です、30ではありません。
```

## ディープダイブ
Python 3.6より前では、`.format()`が文字列を補間する方法でした：

```Python
name = 'Bob'
age = 25
greeting = "こんにちは、{}さん。あなたは{}歳です。".format(name, age)

print(greeting)
```

古い学校のPython（バージョン < 2.6）では、補間に`%`演算子を使用していました。これは直感的ではなく、複数の変数を使っているときにはごちゃごちゃになりがちです：

```Python
name = 'Carol'
age = 35
greeting = "こんにちは、%sさん。あなたは%d歳です。" % (name, age)

print(greeting)
```

より清潔な構文の他に、f-stringsは実行時に評価され、効率的な文字列形式の操作に直接変換されるため、より速いです。 `.format()`と`%`演算子はより多くのステップを含み、遅いです。

## 参照
- [PEP 498 – リテラル文字列補間](https://www.python.org/dev/peps/pep-0498/) f-stringsの公式ドキュメント。
- [Python f-strings](https://realpython.com/python-f-strings/) by Real Pythonでのf-stringsのチュートリアル。
- [Python documentationの.format()メソッド](https://docs.python.org/3/library/stdtypes.html#str.format) 古い`.format()`メソッドによる文字列フォーマッティングを理解するため。
