---
title:                "Elm: ディレクトリの存在を確認する"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜディレクトリの存在を確認するのですか？

ディレクトリの存在を確認することは、特定のシステムやプロジェクトで必要なファイルやデータを保管するために重要です。エルムプログラミングでは、ファイルやディレクトリの操作は頻繁に行われるため、ディレクトリの存在を確認することは非常に重要です。

## 方法

まず、`elm/file`パッケージをインポートします。

```elm
import File
```

次に、`exists`関数を使ってディレクトリの存在を確認することができます。この関数は`Result`型を返し、`Ok`の場合はディレクトリのパスを、`Err`の場合はエラーメッセージを返します。

```elm
-- /foo というディレクトリの存在を確認する例
File.exists "/foo"

-- 出力: Ok "/foo"
```

また、`existsSync`関数を使うことで、同期的に実行し、`Bool`型を返すこともできます。`True`の場合はディレクトリが存在し、`False`の場合は存在しないことを示します。

```elm
-- /bar というディレクトリの存在を確認する例
File.existsSync "/bar"

-- 出力: True
```

## 深堀り

`exists`関数は、指定されたパスにファイルが存在しなかった場合でも、ディレクトリならば`Ok`を返します。そのため、ファイルとディレクトリを区別する必要がある場合は、さらに`isDirectory`関数を使用する必要があります。

```elm
-- /baz というファイルが存在するかどうかを確認する例
File.exists "/baz"
|> Result.andThen
    (\path ->
        case File.isDirectory path of
            True -> Result.Err "This is a directory"
            False -> Result.Ok path
    )

-- 出力: Err "This is a directory"
```

## おすすめ記事

- [Elm公式ドキュメント - File.exists](https://package.elm-lang.org/packages/elm/file/latest/File#exists)
- [Elm公式ドキュメント - File.isDirectory](https://package.elm-lang.org/packages/elm/file/latest/File#isDirectory)
- [Elm中国コミュニティ - Elmファイル操作チュートリアル](https://elm-china.org/t/tutorial-elm-file-operations/687)

## 関連記事を見る