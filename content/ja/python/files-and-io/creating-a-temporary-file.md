---
date: 2024-01-20 17:40:57.036124-07:00
description: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210\u306F\u3001\u30C7\
  \u30FC\u30BF\u3092\u77ED\u671F\u9593\u4FDD\u5B58\u3059\u308B\u305F\u3081\u306B\u4F7F\
  \u308F\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u5B9F\u884C\u4E2D\
  \u306E\u307F\u5FC5\u8981\u306A\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3001\u4F7F\
  \u7528\u3057\u3001\u7D42\u308F\u3063\u305F\u3089\u6D88\u53BB\u3057\u305F\u3044\u6642\
  \u306B\u4F7F\u3046\u3093\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.525161-06:00'
model: gpt-4-1106-preview
summary: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210\u306F\u3001\u30C7\
  \u30FC\u30BF\u3092\u77ED\u671F\u9593\u4FDD\u5B58\u3059\u308B\u305F\u3081\u306B\u4F7F\
  \u308F\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u5B9F\u884C\u4E2D\
  \u306E\u307F\u5FC5\u8981\u306A\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3001\u4F7F\
  \u7528\u3057\u3001\u7D42\u308F\u3063\u305F\u3089\u6D88\u53BB\u3057\u305F\u3044\u6642\
  \u306B\u4F7F\u3046\u3093\u3067\u3059\u3002."
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

## What & Why? (何となぜ？)

一時ファイルの作成は、データを短期間保存するために使われます。プログラムが実行中のみ必要なファイルを作成、使用し、終わったら消去したい時に使うんです。

## How to: (方法)

Pythonでは`tempfile`モジュールを使用して一時ファイルを簡単に作成できます。以下は一時ファイルを作成し、使う例です。

```Python
import tempfile

# 一時ファイルを作成し、書き込みをする
with tempfile.TemporaryFile(mode='w+t') as tf:
    tf.write('Pythonで一時ファイル作成')
    tf.seek(0)  # ファイルの先頭に戻る
    print(tf.read()) # ファイルの内容を読み取る

# ファイルは自動的に削除されます
```

出力：
```
Pythonで一時ファイル作成
```

## Deep Dive (深掘り)

`tempfile`モジュールはPythonの標準ライブラリに含まれています。1999年にPython 1.5.2で追加されました。一時ファイルは、プログラムが終了すると自動で削除されるため、ディスクをクリーンに保つのに役立ちます。`NamedTemporaryFile`はファイル名が必要な場合に使い、`TemporaryFile`はファイル名なしで構いません。他言語にも似た機能を提供するライブラリがありますが、Pythonの`tempfile`は使いやすさで優れています。

## See Also (関連情報)

- [tempfile — Generate temporary files and directories](https://docs.python.org/3/library/tempfile.html) ：Python公式ドキュメント
- [Pythonの標準ライブラリ](https://docs.python.org/3/library/index.html) ：一時ファイル以外の様々なモジュール探索
