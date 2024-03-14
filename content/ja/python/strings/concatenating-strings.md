---
date: 2024-01-20 17:35:41.265436-07:00
description: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\uFF12\u3064\u4EE5\
  \u4E0A\u306E\u6587\u5B57\u5217\u3092\u3064\u306A\u3052\u308B\u3053\u3068\u3067\u3059\
  \u3002\u30B3\u30FC\u30C9\u3092\u30B7\u30F3\u30D7\u30EB\u306B\u3059\u308B\u305F\u3081\
  \u3001\u307E\u305F\u306F\u30E6\u30FC\u30B6\u30FC\u306B\u51FA\u529B\u3059\u308B\u30E1\
  \u30C3\u30BB\u30FC\u30B8\u3092\u52D5\u7684\u306B\u4F5C\u308B\u305F\u3081\u306B\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u6587\u5B57\u5217\u306E\u9023\u7D50\u3092\u3057\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.489099-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\uFF12\u3064\u4EE5\
  \u4E0A\u306E\u6587\u5B57\u5217\u3092\u3064\u306A\u3052\u308B\u3053\u3068\u3067\u3059\
  \u3002\u30B3\u30FC\u30C9\u3092\u30B7\u30F3\u30D7\u30EB\u306B\u3059\u308B\u305F\u3081\
  \u3001\u307E\u305F\u306F\u30E6\u30FC\u30B6\u30FC\u306B\u51FA\u529B\u3059\u308B\u30E1\
  \u30C3\u30BB\u30FC\u30B8\u3092\u52D5\u7684\u306B\u4F5C\u308B\u305F\u3081\u306B\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u6587\u5B57\u5217\u306E\u9023\u7D50\u3092\u3057\
  \u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

文字列の連結とは、２つ以上の文字列をつなげることです。コードをシンプルにするため、またはユーザーに出力するメッセージを動的に作るためにプログラマーは文字列の連結をします。

## How to: (やり方)

Pythonでは文字列を連結する方法は簡単です。以下に例を示します。

```python
# 文字列の加算による連結
greeting = "こんにちは、"
name = "山田さん"
message = greeting + name
print(message)  # 出力: こんにちは、山田さん

# 文字列のformatメソッドによる連結
greeting = "こんにちは、{}"
name = "山田さん"
message = greeting.format(name)
print(message)  # 出力: こんにちは、山田さん

# f-stringによる連結（Python 3.6以上）
name = "山田さん"
message = f"こんにちは、{name}"
print(message)  # 出力: こんにちは、山田さん

# joinメソッドによる複数文字列の連結
words = ["Python", "は", "楽しい"]
sentence = " ".join(words)
print(sentence)  # 出力: Python は 楽しい
```

## Deep Dive (掘り下げ)

歴史的背景として、古いバージョンのPythonでは`+`オペレータや`%`オペレータを使用して文字列を連結していました。しかし、Python 3.6以降、f-stringが導入され、可読性とパフォーマンスの両方で優れた方法とされています。`join`メソッドは、リスト内の多数の文字列を単一の文字列に結合する際に特に効果的です。

実装の詳細では、`+`オペレータはシンプルですが、多数の文字列を連結する際はそれほど効率的ではありません。大量の連結を行う場合、`join`やf-stringを使った方がパフォーマンスが良いです。

連結の代替方法として、リスト内包表記や`str.join`、`str.format`、f-stringなどが挙げられます。これらの方法は、状況に応じて選ぶとよいでしょう。

## See Also (参照)

- [Pythonの公式ドキュメント](https://docs.python.org/3/)
- [PEP 498 -- Literal String Interpolation](https://www.python.org/dev/peps/pep-0498/)
- Pythonの基本的な文字列操作に関するチュートリアル記事やビデオ
- [Stack Overflowの文字列連結に関する質問](https://stackoverflow.com/questions/tagged/string-concatenation+python)
