---
date: 2024-01-20 17:46:12.686581-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.485372-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## How to: (方法)
```Python
# 文字列の部分を抽出する例
text = "こんにちは、Pythonの世界へようこそ！"

# スライスを使って抽出
hello = text[:5]
python_world = text[6:20]

print(hello)          # こんにちは
print(python_world)   # Pythonの世界へ

# 文字列のメソッドを使って抽出
substring = text.split('、')[1].split('！')[0]
print(substring)      # Pythonの世界へ
```

## Deep Dive (深掘り)
昔、文字列を操作する方法は限られていましたが、Pythonではスライスやメソッドが便利です。スライスは`文字列[start:end]`で指定。他の言語のsubstringに相当。`split`メソッドなどは、特定の文字で分けて抽出します。正規表現を使うとさらに高度な抽出も可能ですが、それはもう一つの話。

## See Also (参考資料)
- Python公式ドキュメント: https://docs.python.org/ja/3/library/stdtypes.html#string-methods
- Pythonの正規表現の使い方: https://docs.python.org/ja/3/library/re.html
- Pythonにおける文字列操作のチュートリアル: https://realpython.com/python-strings/
