---
aliases:
- /ja/python/extracting-substrings/
date: 2024-01-20 17:46:12.686581-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u90E8\u5206\u3092\u62BD\
  \u51FA\u3059\u308B\u3053\u3068\u3002\u306A\u305C\uFF1F\u30C7\u30FC\u30BF\u51E6\u7406\
  \u3084\u89E3\u6790\u3067\u91CD\u8981\u306A\u60C5\u5831\u3060\u3051\u3092\u53D6\u308A\
  \u51FA\u3059\u305F\u3081\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.553678
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u90E8\u5206\u3092\u62BD\
  \u51FA\u3059\u308B\u3053\u3068\u3002\u306A\u305C\uFF1F\u30C7\u30FC\u30BF\u51E6\u7406\
  \u3084\u89E3\u6790\u3067\u91CD\u8981\u306A\u60C5\u5831\u3060\u3051\u3092\u53D6\u308A\
  \u51FA\u3059\u305F\u3081\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列から特定の部分を抽出すること。なぜ？データ処理や解析で重要な情報だけを取り出すため。

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
