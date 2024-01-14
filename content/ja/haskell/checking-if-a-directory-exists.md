---
title:                "Haskell: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかをチェックする必要がある理由は様々です。例えば、プログラム内で特定のファイルを処理する前に、そのファイルが存在するかどうかを確認する必要があります。また、ファイルを保存する前に、ファイルが既に存在するかどうかを確認して、上書きを防ぐこともできます。

## 方法

ディレクトリが存在するかどうかを確認するには、`directory`パッケージを使用します。まず、必要なパッケージをインポートします。

```Haskell
import System.Directory
```

次に、`doesDirectoryExist`関数を使用してディレクトリの存在を確認します。

```Haskell
doesDirectoryExist :: FilePath -> IO Bool
```

この関数は、与えられたパスのディレクトリが存在する場合には`True`を返し、存在しない場合には`False`を返します。例えば、`/home/user/Documents`というパスのディレクトリが存在するかどうかを確認する場合、以下のようにコードを書くことができます。

```Haskell
doesDirectoryExist "/home/user/Documents"
```

もしディレクトリが存在する場合は`True`を、存在しない場合は`False`を返します。

## ディープダイブ

`doesDirectoryExist`関数は、実際には`System.Posix.Directory`モジュールの`DirectoryExist`型を使用しています。この型は、`Foreign.C.Error`モジュールからインポートされた`errno_t`を使用して、`IO Bool`の返り値を提供します。このため、ファイルシステム操作において何らかのエラーが発生した場合は、`IO Bool`の返り値は`False`ではなく`throwErrnoIfPathInvalid`関数を使用してエラーをスローします。

## ぜひ参考にしてみてください

**参考リンク：**

1. [Haskellドキュメント - System.Directory](https://hackage.haskell.org/package/directory-1.3.4.1/docs/System-Directory.html)
2. [Haskellドキュメント - System.Posix.Directory](https://hackage.haskell.org/package/directory-1.3.4.1/docs/System-Posix-Directory.html)
3. [Haskellドキュメント - Foreign.C.Error](https://www.haskell.org/onlinereport/haskell2010/haskellch35.html#x42-14900035)
4. [Haskellドキュメント - throwErrnoIfPathInvalid](https://hackage.haskell.org/package/base-4.14.1.0/docs/Foreign-C-Error.html#v:throwErrnoIfPathInvalid)