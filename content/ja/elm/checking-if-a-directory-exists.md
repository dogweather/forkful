---
title:                "Elm: ディレクトリが存在するかどうかを確認する。"
simple_title:         "ディレクトリが存在するかどうかを確認する。"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# なぜ

今日は、Elmプログラミングの世界に深く入り込んでみましょう。最近、ディレクトリが存在するかどうかをチェックする方法について検証を行ったので、その結果を共有したいと思います。はじめに、ディレクトリが存在するかどうかを知ることの重要性についてお話しします。

## ディレクトリの存在を確認する方法

ディレクトリが存在すれば、ファイル操作を行うことができます。Elmでは、FileSystemパッケージを使用することで、ディレクトリの存在を確認することができます。

```Elm
import FileSystem

FileSystem.exists "path/to/directory"
    |> Task.perform checkDirectoryExists

-- ディレクトリが存在しない場合は、Falseが返されます
checkDirectoryExists : Bool -> Cmd msg
checkDirectoryExists exists =
    if exists then
        -- ここでファイル操作を行います
        Cmd.none
    else
        -- エラー処理を行います
        Cmd.none
```

上記のコードでは、Task.perform関数を使用し、ディレクトリの存在を確認しています。Task.perform関数は非同期的な処理を行う際に使用することができます。

## ディープダイブ

ここで、FileSystem.exists関数がどのように実装されているかについて紹介したいと思います。FileSystemモジュールのソースコードを見ると、実際の処理は"scandir"関数によって行われていることがわかります。"scandir"関数は、与えられたパスをスキャンし、その結果をリストとして返す関数です。

また、FileSystem.exists関数は非同期的な処理を行う必要があるため、Task.succeed関数とTask.attempt関数を使用しています。Task.succeed関数は指定した値を持つTaskを作成し、Task.attempt関数はTaskを実行し、結果を受け取る関数を引数に取ります。

# 参考リンク

- [FileSystemパッケージのドキュメント](https://package.elm-lang.org/packages/elm/file/latest/FileSystem) 
- [Google DevelopersによるFileSystem APIの解説](https://developers.google.com/web/updates/2012/08/FileSystem-API?hl=ja) 
- [Elmの非同期処理を行うためのTaskモジュールのドキュメント](https://package.elm-lang.org/packages/elm/core/latest/Task)
 
# 参考文献

- [FileSystemソースコード](https://github.com/elm/file/blob/master/src/FileSystem.elm)
- [存在しないパスを与えた場合の処理](https://github.com/elm/core/blob/1.0.2/src/Native/Utils.js#L106-L108)