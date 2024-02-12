---
title:                "インタラクティブシェル（REPL）の使用"
aliases:
- /ja/elm/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:13:47.274264-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 何となぜ？
Read-Eval-Print Loop（REPL）は、ユーザーの単一の入力を取り、評価し、結果をユーザーに返すシンプルなインタラクティブプログラミング環境です。Elmプログラマーは、REPLを使って簡単な実験、デバッグ、または言語の学習を行います。

## 使い方：
Elmには統合されたREPLは付属していません。しかし、Elmをインストールした後、コマンドラインから`elm repl`を使ってElmセッションを開始できます。

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

このセッションでは、List関数をインポートした後、リスト内の数字を2倍にして、即座に結果を得ました。

## より深く
ElmのREPLは、ElmがWebアプリの生成に焦点を当てたコンパイル言語であるため、PythonやJavaScriptのような他の言語のREPLと比べると制限されているように見えるかもしれません。歴史的に、Elmはスクリプティングやシェルの対話よりも完全なアプリケーションに焦点を合わせてきました。

ElmのREPLへの代替手段には、`elm-live`やオンラインエディタのEllieがあり、ブラウザでリアルタイムにコードの変更を確認できます。

実装に関しては、ElmのREPLは背後でElmコードのスニペットをJavaScriptにコンパイルし、対話的にElmを実行できるようにします。これは、コンパイルステップが不要な解釈言語のREPLとは異なります。ElmのREPLはまた、コア言語を軽量でフォーカスされたものに保つために簡素化されています。

## 参照
- インタラクティビティに関するElmの公式ガイド: https://guide.elm-lang.org/interop/
- Ellie, オンラインのElmプレイグラウンド: https://ellie-app.com/new
- `elm-live`, Elm用の柔軟な開発サーバー: https://www.elm-live.com/
