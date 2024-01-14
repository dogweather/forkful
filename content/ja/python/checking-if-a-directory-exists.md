---
title:                "Python: ディレクトリが存在するかを確認する"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認することには、いくつかの理由があります。例えば、ファイルを保存する場所や、ファイルを読み取る場所を事前に確認する必要がある場合があります。

## 方法

Pythonでディレクトリが存在するかどうかを確認するには、```os.path.exists()```メソッドを使用します。

```Python
import os

# 存在するディレクトリのパスを指定
path = "/Users/myusername/Documents"

# ディレクトリの存在を確認
if os.path.exists(path):
    print("ディレクトリが存在します。")
else:
    print("ディレクトリが存在しません。")
```

もしディレクトリが存在する場合は、"ディレクトリが存在します。"という出力結果が表示されます。もし存在しない場合は、"ディレクトリが存在しません。"という出力結果が表示されます。

## 深堀り

Pythonの```os.path.exists()```メソッドは、指定されたパスが存在するかどうかを確認するだけでなく、ファイルやシンボリックリンクの存在も確認します。もし存在しない場合は、Falseを返します。また、絶対パスや相対パスの両方に対応しています。

## 併せて参照

- [Python os.pathモジュールの公式ドキュメント](https://docs.python.org/ja/3/library/os.path.html)
- [Python os.path.exists()メソッドの公式ドキュメント](https://docs.python.org/ja/3/library/os.path.html#os.path.exists)