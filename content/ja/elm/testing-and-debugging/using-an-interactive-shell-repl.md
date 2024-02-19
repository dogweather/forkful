---
aliases:
- /ja/elm/using-an-interactive-shell-repl/
date: 2024-01-26 04:13:47.274264-07:00
description: "Read-Eval-Print Loop\uFF08REPL\uFF09\u306F\u3001\u30E6\u30FC\u30B6\u30FC\
  \u306E\u5358\u4E00\u306E\u5165\u529B\u3092\u53D6\u308A\u3001\u8A55\u4FA1\u3057\u3001\
  \u7D50\u679C\u3092\u30E6\u30FC\u30B6\u30FC\u306B\u8FD4\u3059\u30B7\u30F3\u30D7\u30EB\
  \u306A\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30D7\u30ED\u30B0\u30E9\u30DF\
  \u30F3\u30B0\u74B0\u5883\u3067\u3059\u3002Elm\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001REPL\u3092\u4F7F\u3063\u3066\u7C21\u5358\u306A\u5B9F\u9A13\u3001\u30C7\
  \u30D0\u30C3\u30B0\u3001\u307E\u305F\u306F\u8A00\u8A9E\u306E\u5B66\u7FD2\u3092\u884C\
  \u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.842487
model: gpt-4-0125-preview
summary: "Read-Eval-Print Loop\uFF08REPL\uFF09\u306F\u3001\u30E6\u30FC\u30B6\u30FC\
  \u306E\u5358\u4E00\u306E\u5165\u529B\u3092\u53D6\u308A\u3001\u8A55\u4FA1\u3057\u3001\
  \u7D50\u679C\u3092\u30E6\u30FC\u30B6\u30FC\u306B\u8FD4\u3059\u30B7\u30F3\u30D7\u30EB\
  \u306A\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30D7\u30ED\u30B0\u30E9\u30DF\
  \u30F3\u30B0\u74B0\u5883\u3067\u3059\u3002Elm\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001REPL\u3092\u4F7F\u3063\u3066\u7C21\u5358\u306A\u5B9F\u9A13\u3001\u30C7\
  \u30D0\u30C3\u30B0\u3001\u307E\u305F\u306F\u8A00\u8A9E\u306E\u5B66\u7FD2\u3092\u884C\
  \u3044\u307E\u3059\u3002"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
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
