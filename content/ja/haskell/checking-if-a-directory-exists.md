---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "Haskell: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？
ディレクトリ存在確認は、あるディレクトリが存在するか否かをチェックするプロセスのことを指します。これはプログラマーが不必要なエラーを防ぎ、効率的にディレクトリへの読み書き操作を行うために行います。

## 方法：
Haskellでは、System.Directory モジュール内の doesDirectoryExist 関数を使用します。以下はその使用例です。

```Haskell
import System.Directory

main = do
    isDir <- doesDirectoryExist "/your/directory/path"
    if isDir
      then putStrLn "Directory exists"
      else putStrLn "Directory does not exist"
```
これを実行すると、指定したパスがディレクトリとして存在する場合は "Directory exists"、存在しない場合は "Directory does not exist" と出力されます。

## 深い掘り下げ
歴史的な背景としては、この関数は GHC 6.4 から利用可能で、その後 Haskell の標準ライブラリに組み込まれています。代替手段としては、手元にST Monadを使用してこの処理を自作することも可能ですが、既に標準ライブラリに存在する関数を使用した方がコードは簡潔になります。実装については、この関数は裏側でOSのシステムコールを呼び出してディレクトリの存在を確認しています。

## 参考文献：
* System.Directory モジュール公式ドキュメンテーション: [http://hackage.haskell.org/package/directory](http://hackage.haskell.org/package/directory)
* HaskellのST Monadについて: [https://wiki.haskell.org/State_Monad](https://wiki.haskell.org/State_Monad)