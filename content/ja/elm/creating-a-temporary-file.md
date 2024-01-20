---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ？

一時的なファイル作成とは、一時的なデータ保管のためにプログラムが取得する静的な場所です。プログラマーは、一時データの保管、デバッグ、または大量のデータを高速に処理するためによく用いられます。

## 実装方法：

残念ながら、Elmプログラミング言語は一時ファイルを作成する直接的な機能をサポートしていません。このため、ElmからJavaScriptへのポートを使用してこの問題を解決する一方できますが、Elmの純粋性が一部失われます。また、サーバーサイドで一時ファイルの作成を取り扱い、Elmはフロントエンドで結果を取得するという役割を果たすことがあります。以下は、このアプローチを示す伪プログラムです。

```Elm
module Main exposing (..)

type alias Model = 
    { tempFilePath : String }

type Msg = 
    TemporaryFileCreated String

init : Model
init = 
    { tempFilePath = "" }

update : Msg -> Model -> Model
update msg model = 
    case msg of 
        TemporaryFileCreated path ->
            { model | tempFilePath = path }
```

## より深く：

一時ファイルの作成は古くから存在しており、UNIX互換のシステムでは `/tmp` ディレクトリが一般的に使用されます。しかし、一時的なデータ保存やデータの追加・削除などにはリスト、キュー、スタックなどのデータ構造も使用できます。Elmでは、effect managerを使用してファイルを操作する機能を開発することも可能ですが、これは非公式であり、安定性と互換性については保証がありません。

## 参考資料：



以上が一時ファイル作成に関する情報です。楽しくElmプログラミングを進めて行きましょう！