---
title:                "コマンドライン引数の読み取り"
date:                  2024-01-20T17:56:02.362360-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
コマンドライン引数の読み取りとは、コンソールからプログラムに情報を渡す手段のことです。プログラマはこれを用いて、プログラムの実行時に設定やデータをカスタマイズします。

## How to:
Elmはフロントエンド開発に特化しているため、直接的なコマンドライン引数の読み取り機能を持っていません。Elmでコマンドラインツールを作る場合はNode.jsと連携する必要があります。以下はNode.jsとElmでコマンドライン引数を扱う例です。

```Elm
-- Node.jsでElmのプログラムを実行するときの例
const { Elm } = require('./Main.elm');

// コマンドライン引数を取得
const args = process.argv.slice(2);

// Elmアプリケーションを開始
const app = Elm.Main.init({
  flags: args
});

// Elmからの出力を待ち受ける
app.ports.output.subscribe(function (data) {
  console.log(data);
});
```

```Elm
-- ElmのMain.elmからの例
port module Main exposing (..)

import Platform

port output : String -> Cmd msg

-- プログラムの開始
main =
    Platform.worker
        { init = init, update = \_ _ -> ( (), Cmd.none ), subscriptions = always Sub.none }

-- 初期化関数、ここでコマンドライン引数を受け取る
init : () -> ( (), Cmd msg )
init _ =
    let
        args =
            Flags.toString
    in
    ( (), output ("Received command line arguments: " ++ args) )
```

これを実行すると、Node.jsからElmプログラムに引数が渡され、Elm側でそれを処理して結果をNode.jsに送り返します。

```bash
$ node myElmApp.js arg1 arg2 arg3
Received command line arguments: arg1,arg2,arg3
```

## Deep Dive
Elmはもともとウェブブラウザで動くアプリケーション向けなので、Node.jsを介して間接的にコマンドライン引数を扱います。過去にはコマンドライン引数をサポートするパッケージが存在したが、現在のバージョンでは直接のサポートはありません。代わりにNode.jsの`process.argv`を使い、Elmに渡します。他の方法としてはElmからコンパイルされたJavaScriptをNode.jsスクリプトと組み合わせる形になります。

## See Also
- Elm公式ガイド: https://guide.elm-lang.org
- Node.jsのコマンドライン引数の読み取り: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- ElmとNode.jsの連携についてさらに詳しく知る: https://elm-lang.org/news/small-assets-without-the-headache
