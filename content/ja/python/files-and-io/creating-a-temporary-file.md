---
date: 2024-01-20 17:40:57.036124-07:00
description: "How to: (\u65B9\u6CD5) Python\u3067\u306F`tempfile`\u30E2\u30B8\u30E5\
  \u30FC\u30EB\u3092\u4F7F\u7528\u3057\u3066\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\
  \u7C21\u5358\u306B\u4F5C\u6210\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u4E00\
  \u6642\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3057\u3001\u4F7F\u3046\u4F8B\u3067\
  \u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.476913-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Python\u3067\u306F`tempfile`\u30E2\u30B8\u30E5\u30FC\u30EB\
  \u3092\u4F7F\u7528\u3057\u3066\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u7C21\u5358\
  \u306B\u4F5C\u6210\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u4E00\u6642\u30D5\
  \u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3057\u3001\u4F7F\u3046\u4F8B\u3067\u3059\u3002"
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

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
