---
title:                "一時ファイルの作成"
html_title:           "Python: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成する理由は、一時的にデータを保存したり、プログラムの一部を一時的に実行するためです。一時ファイルはプログラミングにおいて非常に便利であり、メモリを効率的に使用することができます。

## 作り方

Pythonの標準ライブラリーには、一時ファイルを作成するための `tempfile` モジュールがあります。`NamedTemporaryFile()` 関数を使用することで、一時ファイルを作成することができます。以下は例です。

```Python
import tempfile

# 一時ファイルを作成
temp_file = tempfile.NamedTemporaryFile()

# 一時ファイルにデータを書き込む
temp_file.write("Hello, world!\n")
temp_file.write("This is a temporary file.")

# ファイルを読み込みモードで開く
temp_file.seek(0)
contents = temp_file.read()

# 出力
print(contents.decode())

# 一時ファイルを閉じる
temp_file.close()

# 出力
# Hello, world!
# This is a temporary file.
```

## 深堀り

`tempfile` モジュールには、一時ファイルの作成方法をより細かく制御する方法もあります。例えば、 `NamedTemporaryFile()` 関数でファイルを作成する際に、 `delete=False` を指定することで、一時ファイルをプログラムが終了するまで削除しないようにすることができます。また、 `tempfile` モジュールには `TemporaryDirectory()` 関数もあり、一時的なディレクトリを作成することもできます。

## See Also

- [Python公式ドキュメント - tempfileモジュール](https://docs.python.org/ja/3/library/tempfile.html)
- [Real Python - Working with Temporary Files and Directories in Python](https://realpython.com/python-tempfile/)