---
title:                "Elm: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# なぜ作るの？

作りモノをする際、時に一時的なファイルが必要になります。例えば、データ保存やバックアップ、あるいは一時的な処理のために作ることがあります。Elmでは、一時的なファイルの作成も簡単にできます。

## 作り方

まず、一時的なファイルを作るためには、Elmの`Graphics.Input`を使います。このモジュールには`createFile`という関数が用意されており、引数にファイル名とデータを渡すことで、一時的なファイルを作ることができます。

```Elm
import Graphics.Input

tempFile = "temp.txt" -- 作りたい一時的なファイル名
data = "これは一時的なファイルのコンテンツです。"

file = Graphics.Input.createFile tempFile data

case file of
    Ok file -> "一時的なファイルが作成されました。"
    Err err -> "一時的なファイルの作成に失敗しました。" ++ err
```

もし、ファイル名の指定を省略したい場合は、`Graphics.Input.guid()`を使うことで、ランダムなファイル名を取得することができます。

```Elm
import Graphics.Input

data = "これは一時的なファイルのコンテンツです。"

file = Graphics.Input.createFile (Graphics.Input.guid()) data

case file of
    Ok file -> "一時的なファイルが作成されました。"
    Err err -> "一時的なファイルの作成に失敗しました。" ++ err
```

## 深堀り

一時的なファイルの作成には、`Graphics.Input`モジュール以外にも、`Dict`や`List`といった他のモジュールを組み合わせて使うこともできます。また、作成した一時的なファイルを後で削除する際には、`Graphics.Input.deleteFile`を使用します。

```Elm
import Graphics.Input
import Dict exposing (Dict)

path = "temp.txt"
data = "これは一時的なファイルのコンテンツです。"

file = Graphics.Input.createFile path data

case file of
    Ok file -> "一時的なファイルが作成されました。"
    Err err -> "一時的なファイルの作成に失敗しました。" ++ err

-- ファイルの削除
result = Graphics.Input.deleteFile path

case result of
    Ok _ -> "一時的なファイルが削除されました。"
    Err err -> "一時的なファイルの削除に失敗しました。" ++ err
```

# 参考リンク

- [Elm公式ガイド: Graphics.Input](https://guide.elm-lang.org/architecture/effects/temp_files.html)
- [Elm公式ガイド: Dictモジュールの使い方](https://guide.elm-lang.org/core_language.html#dict)
- [Elm公式ガイド: Listモジュールの使い方](https://guide.elm-lang.org/core_language.html#list)