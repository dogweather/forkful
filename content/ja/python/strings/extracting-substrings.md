---
date: 2024-01-20 17:46:12.686581-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.440211-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6614\u3001\u6587\u5B57\u5217\u3092\u64CD\u4F5C\u3059\u308B\
  \u65B9\u6CD5\u306F\u9650\u3089\u308C\u3066\u3044\u307E\u3057\u305F\u304C\u3001Python\u3067\
  \u306F\u30B9\u30E9\u30A4\u30B9\u3084\u30E1\u30BD\u30C3\u30C9\u304C\u4FBF\u5229\u3067\
  \u3059\u3002\u30B9\u30E9\u30A4\u30B9\u306F`\u6587\u5B57\u5217[start:end]`\u3067\u6307\
  \u5B9A\u3002\u4ED6\u306E\u8A00\u8A9E\u306Esubstring\u306B\u76F8\u5F53\u3002`split`\u30E1\
  \u30BD\u30C3\u30C9\u306A\u3069\u306F\u3001\u7279\u5B9A\u306E\u6587\u5B57\u3067\u5206\
  \u3051\u3066\u62BD\u51FA\u3057\u307E\u3059\u3002\u6B63\u898F\u8868\u73FE\u3092\u4F7F\
  \u3046\u3068\u3055\u3089\u306B\u9AD8\u5EA6\u306A\u62BD\u51FA\u3082\u53EF\u80FD\u3067\
  \u3059\u304C\u3001\u305D\u308C\u306F\u3082\u3046\u4E00\u3064\u306E\u8A71\u3002"
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
