---
title:                "デバッグ出力を表示する"
date:                  2024-01-20T17:53:39.642992-07:00
model:                 gpt-4-1106-preview
simple_title:         "デバッグ出力を表示する"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
デバッグ出力とは、コード実行中の変数や進行状況を表示することです。これを行う理由は、バグの原因を見つけたり、コードの挙動を理解するためです。

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
