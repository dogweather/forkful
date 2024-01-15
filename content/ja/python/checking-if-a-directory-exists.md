---
title:                "ディレクトリの存在を確認する"
html_title:           "Python: ディレクトリの存在を確認する"
simple_title:         "ディレクトリの存在を確認する"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# なぜ
ディレクトリが存在するかどうかを確認する必要がある理由は、アプリケーションの中でファイルやデータを処理する際に、事前にその場所が存在するかを確認することで、エラーを回避することができるからです。

## 使い方
```Python
import os
directory_name = input("ディレクトリ名を入力してください： ")
if os.path.isdir(directory_name):
    print("ディレクトリが存在します。")
else:
    print("ディレクトリが存在しません。")
```
入力されたディレクトリ名が存在するかどうかを確認し、結果を出力する簡単なコード例です。

```Python
import os
directory_name = "test_directory"
if os.path.isdir(directory_name):
    print("ディレクトリが存在します。")
else:
    print("ディレクトリが存在しません。")
```
上記コードの実行結果は、次のようになります。
```
ディレクトリが存在しません。
```
ディレクトリが存在しないので、`else`ブロックの`print`文が実行されます。

## ディープダイブ
実際には、ディレクトリが存在するどうかだけでなく、そのディレクトリが読み取り可能かどうかやパーミッションの問題など、さまざまな場合分けが必要になることもあります。また、仮想環境などを使用している場合は、ディレクトリのパスが変わる可能性もあるため、動的にパスを取得する方法も考慮する必要があります。

# もっと詳しく知る
- [Python公式ドキュメント: `os.path`モジュール](https://docs.python.org/ja/3/library/os.path.html)
- [Tec Admin: Check if a File or Directory Exists in Python](https://tecadmin.net/check-file-directory-exists-python/)
- [Stack Overflow: Check if a directory exists and create it if necessary](https://stackoverflow.com/questions/273192/how-can-i-safely-create-a-nested-directory-in-python)