---
title:                "テキストファイルの読込み"
html_title:           "Elm: テキストファイルの読込み"
simple_title:         "テキストファイルの読込み"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

あなたがなぜテキストファイルを読みたいのか、その理由は様々かもしれません。例えば、データ分析をするためにファイルから情報を取得したい場合や、設定ファイルを読み込んで動作を変更したい場合などが考えられます。今回は、テキストファイルを読み込む方法をElmについて説明します。

## 方法

テキストファイルを読み込むには、Elmの組み込み関数である`File.read`を使用します。まず、ファイル名を指定してファイルオブジェクトを作成し、`File.read`に渡します。その後、`Task.attempt`を使って非同期処理を行うことで、ファイルの中身を取得することができます。以下のコードは、`Settings.txt`というファイルを読み込み、その中身をコンソールに表示する例です。

```elm
import File exposing (read)
import Task exposing (attempt)

file : File

file =
    File.Name "Settings.txt"

task =
    read file

view task =
    Html.none

main =
    view <| Task.attempt Debug.log task
```

このようにして、テキストファイルを読み込むことができます。

## ディープダイブ

`File.read`は、エンコーディングを指定することも可能です。デフォルトではUTF-8が使用されますが、Windows環境であればUTF-16やShift-JISなどに変更することもできます。また、ファイルを直接操作するのではなく、リクエストを生成してサーバーに送信することもできます。これらの詳細については、公式ドキュメントを参照してください。

## 参考リンク

- [Elm Official Documentation](https://elm-lang.org/docs)
- [Elm File Module](https://elm-lang.org/docs/working-with-files)
- [Elm File Utilities](https://package.elm-lang.org/packages/elm/file/latest/)