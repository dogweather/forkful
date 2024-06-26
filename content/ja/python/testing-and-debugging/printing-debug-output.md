---
date: 2024-01-20 17:53:39.642992-07:00
description: "How to: (\u65B9\u6CD5) Python\u3067\u306F`print()`\u95A2\u6570\u3092\
  \u4F7F\u3063\u3066\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u884C\u3044\u307E\u3059\
  \u3002\u7C21\u5358\u306A\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.456758-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Python\u3067\u306F`print()`\u95A2\u6570\u3092\u4F7F\u3063\
  \u3066\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u884C\u3044\u307E\u3059\u3002\u7C21\
  \u5358\u306A\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
weight: 33
---

## How to: (方法)
Pythonでは`print()`関数を使ってデバッグ出力を行います。簡単な例を見てみましょう。

```Python
# 変数の値を出力
number = 42
print(f"number の値は: {number}")

# エラーを特定するための出力
for i in range(5):
    print(f"ループの {i} 回目")
    # 何かしらの処理...
```
出力結果:
```
number の値は: 42
ループの 0 回目
ループの 1 回目
ループの 2 回目
ループの 3 回目
ループの 4 回目
```

## Deep Dive (深い潜水)
デバッグ出力の概念は、プログラミングの始まりと共に存在していました。古い時代には、紙のテープに穴を開けることでデバッグ情報を出力していました。今では、`print()`関数だけでなく、ログライブラリ（`logging`モジュール等）でより柔軟なデバッグ出力をする手法もあります。

`print()`はシンプルで直接的な方法ですが、本番環境での出力には適していません。そこで例えば`logging`モジュールが使われます。ログレベルを設定でき、デバッグ、情報、警告、エラー、致命的エラーを区別して出力することができます。

例:

```Python
import logging

logging.basicConfig(level=logging.DEBUG)
logging.debug("これはデバッグメッセージです")
logging.info("これは情報メッセージです")
logging.warning("これは警告メッセージです")
```

実装時は、変数などの動的な情報を出力するために、文字列の書式設定機能をよく使用します。Python 3.6以上では、フォーマット済み文字列リテラル（f-strings）を使うと便利で読みやすいコードになります。

## See Also (関連項目)
- [`print()` function documentation](https://docs.python.org/3/library/functions.html#print)
- [Logging HOWTO](https://docs.python.org/3/howto/logging.html)
- [Python 3's f-Strings: An Improved String Formatting Syntax (Guide)](https://realpython.com/python-f-strings/)
