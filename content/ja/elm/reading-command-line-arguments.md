---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？
コマンドライン引数を読み取るとは、プログラムがユーザーからの入力(引数)をコマンドラインから受け取ることを指します。これは、一般的にコンフィギュレーションの調整やプログラムの動作を変更するために行われます。

## 実行方法：
残念ながら、現在のElm(0.19.1)では、純粋関数型言語としての特性上、直接コマンドライン引数を読み込む機能は提供されていません。

ただし、JavaScriptとの相互運用性を利用してワークアラウンドすることは可能です。ElmからJavaScriptにデータを受け渡す例を見てみましょう：
```Elm
port module Main exposing (main)

import Html exposing (Html, text)
import Browser
import Ports exposing (Cmds)

port toJs : Cmds.Cmd String -> Cmd msg

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
    ...
    toJs(waitingCmd)
```
この例では、`toJs`のポートを通じてJavaScriptに"waitingCmd"メッセージを送信しています。

## ディープダイブ：
**歴史的背景**：Elmは完全に静的な型システムと純粋な関数型の特性を持つ言語です。このため、古典的な意味でのコマンドライン引数の読み取りはサポートされていません。

**代替手段**：上記の`port`を利用した方法以外にも、Elmがビルドされて実行されるエンド環境（通常はJavaScriptエンジン）がコマンドライン引数を受け取り、必要に応じてElmに渡すことも可能です。

**実装詳細**：引数を渡すためには、Elmのポートを使用してJavaScriptと双方向に通信する必要があります。ElmのポートはJavaScriptと非同期通信を行うためのAPIです。しかし、同期通信を実現するためには、JavaScript側でプロミスや非同期関数を用いて対応する必要があります。

## 参照元：
Elmの公式ガイドには、ポートを使ったデータ交換について詳しく説明されています。詳しくは次のリンクをご覧ください：[https://guide.elm-lang.jp/interop/ports.html](https://guide.elm-lang.jp/interop/ports.html)