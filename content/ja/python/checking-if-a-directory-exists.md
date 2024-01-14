---
title:                "Python: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ
ディレクトリが存在するかどうかを確認するのは、プログラミングにおける重要なタスクです。特定のディレクトリが存在するかどうかを確認することにより、ファイルを安全に操作したり、問題を修正したりすることができます。

## 方法
```Python
import os

# 指定したパスのディレクトリが存在するかどうかを確認する
print(os.path.exists("/path/to/directory"))

# 相対パスを使用してディレクトリがあるかどうかを確認する
print(os.path.isdir("directory_name"))

# カレントディレクトリの下に新しいディレクトリを作成する
os.mkdir("new_directory")

# 作成したディレクトリが存在するかどうかを確認する
print(os.path.exists("new_directory"))
```

出力：
```
True
True
True
```

## ディープダイブ
ディレクトリが存在するかどうかを確認する方法は、Pythonにおいては非常に簡単です。`os`モジュールを使用して、様々な方法でディレクトリの存在を確認することができます。例えば、`os.path.exists()`関数は、指定したパスにファイルやディレクトリが存在するかどうかを確認することができます。また、相対パスや、カレントディレクトリからの相対位置でディレクトリが存在するかどうかを確認することも可能です。ディレクトリの存在を確認することで、ファイルの移動やコピーを安全に行うことができます。

## 参考リンク
[Pythonのosモジュールドキュメント (英語)](https://docs.python.org/3/library/os.html)