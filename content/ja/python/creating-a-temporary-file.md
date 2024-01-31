---
title:                "一時ファイルの作成"
date:                  2024-01-20T17:40:57.036124-07:00
model:                 gpt-4-1106-preview
simple_title:         "一時ファイルの作成"

category:             "Python"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

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
