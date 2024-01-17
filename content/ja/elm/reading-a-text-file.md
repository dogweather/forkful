---
title:                "テキストファイルの読み込み"
html_title:           "Elm: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何かとは？
文書ファイルを読み込むとは何か、プログラマーがそれをする理由は何かについて説明します。

## 方法：
```Elm
import File exposing (readFile)
import Task exposing (attempt)
```

文書ファイルを読み込むには、Elmの```File```モジュールの```readFile```メソッドを使います。ファイルを読み込むと、タスクが返されます。そのタスクを実行することで、読み込んだファイルの内容を取得することができます。例えば、```Task.attempt```を使ってエラーハンドリングを行うことができます。

## 深堀り：
プログラムによっては、ファイルを直接読み込まずに、HTTPリクエストを使用してサーバーからデータを取得することもできます。また、より高度なファイル読み込み機能を持つライブラリを使用することもできます。実際に、Elmの標準ライブラリではない、サードパーティのライブラリがあります。これらのライブラリは、ファイルの扱い方についてさらに多くの情報を提供しています。

## 関連情報：
- [Elmガイドブック](http://guide.elm-lang.org/effects/file.html)
- [Elmドキュメンテーション - Fileモジュール](https://package.elm-lang.org/packages/elm/file/latest/)