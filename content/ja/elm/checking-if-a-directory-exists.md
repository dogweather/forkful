---
title:    "Elm: ディレクトリが存在するかどうかを確認する"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ
あるディレクトリが存在するかどうかをチェックすることが重要な理由は、プログラムが目的のファイルを正しく読み込むためです。もしファイルが存在しなければ、プログラムは意図しない動作をするかもしれません。 そのため、プログラマーはチェック機能を含めることで、プログラムの信頼性を高めることができます。

## ハウツー
```Elm
import File

directoryExists : String -> Cmd msg
directoryExists path =
    File.exists path
        |> Task.perform (\_ -> DirectoryExists) (\_ -> DirectoryDoesNotExist)
```
このコード例では、`File.exists`を使って指定したパスのファイルが存在するかどうかをチェックしています。これはコマンドを返すため、`Task.perform`を使って結果を受け取る必要があります。もしファイルが存在すれば、`DirectoryExists`コマンドを実行し、そうでなければ`DirectoryDoesNotExist`コマンドを実行します。

## 深堀り
ディレクトリの存在をチェックする際、注意しなければならない点があります。それは、ディレクトリのパスは常に最後が`/`で終わるようにする必要があるということです。なぜなら、`File.exists`が内部的にディレクトリかどうかの判断を`String`の最後の文字が`/`であるかどうかで行うからです。

例えば、`File.exists "/Users/Bob/Documents"`を実行しても、ディレクトリが存在したとしても`DirectoryDoesNotExist`が返ってきてしまいます。これは渡したパスが `"/Users/Bob/Documents/"`のように最後が`/`で終わっていないためです。

そのため、`directoryExists`関数を使う際は、常にパスの最後に`/`を付けておくようにしましょう。

## 関連記事

- Elm ドキュメント：https://guide.elm-lang.jp/core_libraries/file.html
- Elm フォーラムのディレクトリ存在チェックに関するスレッド：https://discourse.elm-lang.org/t/elms-directory-exists-function/3941
- Elm フォーラムのディレクトリ存在チェックの仕組みに関するスレッド：https://discourse.elm-lang.org/t/how-to-check-if-a-directory-exists/5693