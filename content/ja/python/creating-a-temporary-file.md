---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

---
# 一時ファイル生成のPythonプログラミング
---

## 何 & なぜ？
**一時ファイル（temporary file）**はその名の通り、一時的に使用する目的で生成され、通常プログラム終了時に自動的に消去されるファイルです。これにより、大量のデータを一時的に格納したり、異なるプログラム間でデータを交換したりする際に、メモリを圧迫することなく安全に操作することができます。

## どうやって：
Pythonでは一時ファイルの生成は非常に単純です。以下にサンプルコードを示します。

```Python
import tempfile

# 一時ファイルの生成
temp = tempfile.TemporaryFile()

# データの書き込み
temp.write(b'Sample data')

# 書き込んだデータを読み出すため、ファイルの先頭に移動
temp.seek(0)

# データの読み出し
print(temp.read()) # Output: b'Sample data'

# ファイルのクローズ（この時点で一時ファイルは自動的に消去されます）
temp.close()
```

## ディープダイブ：
一時ファイルの概念はUNIX系システムから始まり、その後多くのプログラミング言語に採用されました。Pythonでは`tempfile`モジュールを通じて一時ファイルを簡単に管理できます。

一方、`tempfile.SpooledTemporaryFile`を使用すると、ファイルサイズがある閾値（デフォルト10000バイト）以下のときはメモリ上（RAM）にデータを保持し、閾値を超えると自動的にディスクに書き込む一時ファイルを使用することができます。

また、`tempfile.mkstemp`は一時ファイルを安全に生成するための関数で、生成した一時ファイルのパスを知る必要がある場合や、自動的に消去されない一時ファイルを生成する場合に使用します。

## 参考資料：
一時ファイルの更なる詳細については、以下のリンクをご参照ください。

1. Python公式ドキュメンテーション：[tempfile - Temporary File System Objects](https://docs.python.org/3/library/tempfile.html)
2. StackOverflow: [What is the purpose of the python tempfile library?](https://stackoverflow.com/questions/45101217/what-is-the-purpose-of-the-python-tempfile-library)