---
title:                "Haskell: ディレクトリが存在するかどうかを確認する"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

あなたはHaskellプログラマーですか？そしてあなたはHaskellでディレクトリが存在するかどうかをチェックする方法を知りたいですか？それでは、この記事を読み続けてください！

## Why 
ディレクトリが存在するかどうかをチェックすることは、コードが期待どおりに動作するかどうかを確認する重要な手段です。ディレクトリが存在するかどうかをチェックすることで、コードがそこで必要なファイルやデータを見つけることができるかを確認することができます。これにより、プログラムが意図した動作をするかどうかを保証することができます。

## How To
それでは、実際にHaskellでディレクトリが存在するかどうかをチェックする方法を見ていきましょう。はじめに、System.Directoryモジュールをインポートします。

```Haskell
import System.Directory
```

次に、`doesDirectoryExist`関数を使用して、ディレクトリが存在するかどうかをチェックします。この関数は、ディレクトリのパスを引数として受け取り、Bool値を返します。

```Haskell
-- ディレクトリのパス
let path = "users/apps/my_app"

-- ディレクトリが存在するかどうかをチェック
let exists = doesDirectoryExist path

-- Bool値を表示
print exists
```

もしディレクトリが存在する場合は`True`が、存在しない場合は`False`が返されます。これで、あなたは簡単にHaskellでディレクトリが存在するかどうかをチェックすることができます。

## Deep Dive
Haskellでディレクトリが存在するかどうかをチェックする方法について深く掘り下げると、`doesDirectoryExist`関数が使用するのは、`getDirectoryContents`関数という裏側の関数であることがわかります。これは、引数として与えられたディレクトリの内容をリストとして返す関数です。そのため、`doesDirectoryExist`関数が`True`を返すかどうかは、実際にディレクトリ内に何か存在するかどうかを確認していることになります。

## See Also
- [HaskellのSystem.Directoryモジュールドキュメント](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Haskellでディレクトリを作成する方法](https://www.haskell.org/hoogle/?hoogle=directory)
- [Haskellでファイルを移動させる方法](https://www.haskell.org/hoogle/?hoogle=directory)