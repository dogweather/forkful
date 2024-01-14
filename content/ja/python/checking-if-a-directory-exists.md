---
title:    "Python: デーブレースが存在するかどうかを確認する"
keywords: ["Python"]
---

{{< edit_this_page >}}

## なぜディレクトリの存在をチェックするのか

プログラミングをする際、ファイルやディレクトリを操作することがあります。その際に、必ずそのファイルやディレクトリが存在しているかどうかを確認する必要があります。そこで今回は、Pythonでディレクトリの存在をチェックする方法について紹介します。

## チェック方法

まずは、次のように`os`モジュールをインポートします。

```Python
import os
```

次に、`os.path.exists()`メソッドを使ってディレクトリの存在をチェックすることができます。例えば、`/Users/username/Desktop`というディレクトリの存在をチェックする場合は、以下のようにコードを書きます。

```Python
if os.path.exists("/User/username/Desktop"):
    print("Desktopディレクトリは存在します。")
else:
    print("Desktopディレクトリは存在しません。")
```

出力結果は以下のようになります。

```
Desktopディレクトリは存在します。
```

もし存在しないディレクトリを指定した場合は、次のようになります。

```Python
if os.path.exists("/User/username/Documents"):
    print("Documentsディレクトリは存在します。")
else:
    print("Documentsディレクトリは存在しません。")
```

出力結果は以下のようになります。

```
Documentsディレクトリは存在しません。
```

## ディープダイブ

`os.path.exists()`メソッドは、指定したファイルやディレクトリが存在する場合は`True`を返し、存在しない場合は`False`を返します。このメソッドは、フォルダやファイルの存在をチェックするだけでなく、Pythonのファイル操作やプログラムの実行時にも役立ちます。

## 参考リンク

- [Python公式ドキュメント - os.path.exists()](https://docs.python.org/ja/3/library/os.path.html#os.path.exists)
- [プログラミング初心者のためのPython- os.path.exists()の使い方](https://www.sejuku.net/blog/68393)
- [Pythonでファイルの存在をチェックする方法](https://codechord.com/2015/10/python-check-if-file-exists/)