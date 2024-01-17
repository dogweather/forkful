---
title:                "ディレクトリが存在するかどうかをチェックする"
html_title:           "Haskell: ディレクトリが存在するかどうかをチェックする"
simple_title:         "ディレクトリが存在するかどうかをチェックする"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

インフォーマルなトーンと冗長な表現を避け、分かりやすく要点を伝えることを心がけました。

## 何か？何故か？

ディレクトリが存在するかどうかを確認することは、プログラマーにとって重要なことです。これは、プログラムが正しく動作するために必要なファイルが存在するかどうかを確認するために行われます。

## 方法：

まず、Haskellの"System.Directory"モジュールをインポートします。次に、"doesDirectoryExist"関数を使用して、チェックしたいディレクトリのパスを指定します。この関数は、ディレクトリが存在した場合は"True"を、存在しなかった場合は"False"を返します。

```Haskell
import System.Directory

main = do
  let path = "Documents/MyFiles"
  exist <- doesDirectoryExist path
  if exist
    then putStrLn "指定したディレクトリは存在します。"
    else putStrLn "指定したディレクトリは存在しません。"
```

```Haskell
出力：
指定したディレクトリは存在しません。
```

## 深い掘り下げ：

プログラミング言語によっては、ディレクトリの存在を確認するための別の方法があります。例えば、Bashならば、"test -d"コマンドを使用することができます。しかし、Haskellでは、"System.Directory"モジュールを使用することで、簡単にディレクトリの存在を確認することができます。

また、"System.Directory"モジュールには、ディレクトリを作成したり、削除したりするための関数もあります。これらの関数を使用することで、プログラムの中でディレクトリを操作することができます。

## 関連情報：
- [Hackage - System.Directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Hoogle - doesDirectoryExist](https://hoogle.haskell.org/?hoogle=doesDirectoryExist)