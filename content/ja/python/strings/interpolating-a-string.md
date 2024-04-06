---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:24:03.388408-07:00
description: "\u65B9\u6CD5\uFF1A Python 3.6\u4EE5\u4E0A\u3067\u306F\u3001f-strings\u3092\
  \u4F7F\u7528\u3057\u3066\u6587\u5B57\u5217\u3092\u88DC\u9593\u3067\u304D\u307E\u3059\
  \u3002\u4EE5\u4E0B\u306E\u65B9\u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:37:49.820009-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Python 3.6\u4EE5\u4E0A\u3067\u306F\u3001f-strings\u3092\
  \u4F7F\u7528\u3057\u3066\u6587\u5B57\u5217\u3092\u88DC\u9593\u3067\u304D\u307E\u3059\
  \u3002\u4EE5\u4E0B\u306E\u65B9\u6CD5\u3067\u3059\uFF1A."
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

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
