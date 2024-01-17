---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "Python: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
ディレクトリが存在するかどうかをチェックすることは、プログラマーがプログラムの実行中に必要なファイルが存在することを確認するために行うものです。これにより、プログラムがエラーなく実行されることが保証されます。

## How to:
Pythonでは、以下のようにしてディレクトリが存在するかどうかをチェックすることができます。

```Python
import os # osモジュールをインポート

# os.path.exists()を使用してディレクトリが存在するかどうかをチェック
if os.path.exists("directory"):
    print("ディレクトリが存在します。")
else:
    print("ディレクトリは存在しません。")
```

実行結果:

```
ディレクトリが存在します。
```

## Deep Dive:
プログラミングの歴史を振り返ると、ディレクトリの存在チェックは古くから使われていた技術です。しかし、Pythonでは簡単に実装できるように、osモジュールにos.path.exists()という関数が用意されています。

他の言語では、ディレクトリの存在チェックには様々な方法があります。例えば、JavaではFileクラスのexists()メソッドを使うことができます。

ディレクトリの存在チェックは、プログラムの実行中にファイルが存在することを確認するだけではなく、ファイルの読み込みや書き込みなどの処理に必要不可欠です。

## See Also:
- [osモジュールの公式ドキュメント](https://docs.python.org/ja/3/library/os.html)
- [JavaのFileクラス](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)