---
title:                "ディレクトリが存在するかどうかをチェックする"
html_title:           "Elm: ディレクトリが存在するかどうかをチェックする"
simple_title:         "ディレクトリが存在するかどうかをチェックする"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## やること & その理由
ディレクトリが存在するかどうかを確認することは、プログラマーがそのディレクトリ内のファイルにアクセスできるかどうかを判断するための方法です。これにより、プログラムが想定通りに動作するかどうかを確認できます。

## 方法:
以下のコード例を参考に、どのようにディレクトリの存在を確認するかを学びましょう。

```Elm
import File

-- ディレクトリが存在するかを確認する関数
folderExists : String -> Task x Bool
folderExists path =
  File.exists path True False
```

上記のコードを実行すると、指定したパスにディレクトリが存在する場合は「True」、存在しない場合は「False」が返されます。

## 深堀り:
ディレクトリの存在を確認する方法には、上記のようにElmのFileパッケージを使用する方法以外にも、より低レベルの方法があります。例えば、ファイルシステムに直接アクセスするNativeコードを使用する方法や、サードパーティのライブラリを利用する方法などがあります。

## さらに参考:
- [ElmのFileパッケージドキュメント](https://package.elm-lang.org/packages/elm/file/latest/)
- [ファイルシステムにアクセスするためのNativeコード](https://elmprogramming.com/file-system-elm-3.html)
- [サードパーティのライブラリを使ったファイル操作の方法](https://github.com/elm/node#filesystem-operations)