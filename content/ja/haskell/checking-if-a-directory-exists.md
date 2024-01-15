---
title:                "ディレクトリが存在するかどうかを調べる"
html_title:           "Haskell: ディレクトリが存在するかどうかを調べる"
simple_title:         "ディレクトリが存在するかどうかを調べる"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ？

ディレクトリが存在するかどうかを確認することは、プログラマーがファイルやデータを正しく処理するために重要なステップです。特に、ファイルを操作するプログラムを開発する場合には、ディレクトリが存在するかどうかを確認することは欠かせません。この記事では、Haskellでディレクトリの存在を確認する方法を紹介します。

## 方法

まずは、Haskellでディレクトリの存在を確認する方法を見てみましょう。ディレクトリが存在するかどうかを確認するには、`doesDirectoryExist`関数を使用します。以下のコード例では、`directoryPath`変数に対象のディレクトリのパスを指定し、`doesDirectoryExist`関数に渡します。

```Haskell
import System.Directory

main = do
  let directoryPath = "/home/user/example_directory"
  dirExists <- doesDirectoryExist directoryPath
  print dirExists
```

上記のコードを実行すると、`True`または`False`が出力されます。これにより、指定したディレクトリが存在するかどうかが確認できます。

## ディープダイブ

`doesDirectoryExist`関数は、`System.Directory`モジュールに属しています。このモジュールには、ディレクトリに関するさまざまな便利な関数が収録されています。例えば、`createDirectory`関数を使用すると、指定したディレクトリを作成することができます。また、`getDirectoryContents`関数を使用すると、指定したディレクトリ内のファイルやディレクトリの一覧を取得することができます。

さらに、Haskellでは例外処理を行うための`try`関数が提供されています。`doesDirectoryExist`関数と組み合わせることで、指定したディレクトリが存在しなかった場合に例外処理を行うことができます。

詳しくは、Haskellの公式ドキュメントを参考にしてください。

## はたして

以上で、Haskellでディレクトリの存在を確認する方法を紹介しました。ディレクトリ操作は、プログラミングにおいて重要な一部です。ぜひ、実際に手を動かしながら学んでみてください。

## 参考リンク

- [Haskell公式サイト](https://www.haskell.org/)
- [System.Directoryモジュールのドキュメント](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [例外処理についてのHaskellWikiの記事](https://wiki.haskell.org/Error_messages_and_Exceptions#Exceptions_in_Haskell)