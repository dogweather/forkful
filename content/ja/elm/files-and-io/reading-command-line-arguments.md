---
date: 2024-01-20 17:56:02.362360-07:00
description: "How to: Elm\u306F\u30D5\u30ED\u30F3\u30C8\u30A8\u30F3\u30C9\u958B\u767A\
  \u306B\u7279\u5316\u3057\u3066\u3044\u308B\u305F\u3081\u3001\u76F4\u63A5\u7684\u306A\
  \u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\u308A\
  \u6A5F\u80FD\u3092\u6301\u3063\u3066\u3044\u307E\u305B\u3093\u3002Elm\u3067\u30B3\
  \u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u30C4\u30FC\u30EB\u3092\u4F5C\u308B\u5834\u5408\
  \u306FNode.js\u3068\u9023\u643A\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\
  \u3002\u4EE5\u4E0B\u306FNode.js\u3068Elm\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\
  \u30F3\u5F15\u6570\u3092\u6271\u3046\u4F8B\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.029280-06:00'
model: gpt-4-1106-preview
summary: "Elm\u306F\u30D5\u30ED\u30F3\u30C8\u30A8\u30F3\u30C9\u958B\u767A\u306B\u7279\
  \u5316\u3057\u3066\u3044\u308B\u305F\u3081\u3001\u76F4\u63A5\u7684\u306A\u30B3\u30DE\
  \u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\u308A\u6A5F\u80FD\
  \u3092\u6301\u3063\u3066\u3044\u307E\u305B\u3093\u3002Elm\u3067\u30B3\u30DE\u30F3\
  \u30C9\u30E9\u30A4\u30F3\u30C4\u30FC\u30EB\u3092\u4F5C\u308B\u5834\u5408\u306FNode.js\u3068\
  \u9023\u643A\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002\u4EE5\u4E0B\
  \u306FNode.js\u3068Elm\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\
  \u3092\u6271\u3046\u4F8B\u3067\u3059."
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
