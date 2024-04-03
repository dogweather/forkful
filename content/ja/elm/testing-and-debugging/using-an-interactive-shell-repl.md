---
date: 2024-01-26 04:13:47.274264-07:00
description: "\u4F7F\u3044\u65B9\uFF1A Elm\u306B\u306F\u7D71\u5408\u3055\u308C\u305F\
  REPL\u306F\u4ED8\u5C5E\u3057\u3066\u3044\u307E\u305B\u3093\u3002\u3057\u304B\u3057\
  \u3001Elm\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u305F\u5F8C\u3001\u30B3\
  \u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u304B\u3089`elm repl`\u3092\u4F7F\u3063\u3066\
  Elm\u30BB\u30C3\u30B7\u30E7\u30F3\u3092\u958B\u59CB\u3067\u304D\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.010411-06:00'
model: gpt-4-0125-preview
summary: "Elm\u306B\u306F\u7D71\u5408\u3055\u308C\u305FREPL\u306F\u4ED8\u5C5E\u3057\
  \u3066\u3044\u307E\u305B\u3093\u3002\u3057\u304B\u3057\u3001Elm\u3092\u30A4\u30F3\
  \u30B9\u30C8\u30FC\u30EB\u3057\u305F\u5F8C\u3001\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\
  \u30F3\u304B\u3089`elm repl`\u3092\u4F7F\u3063\u3066Elm\u30BB\u30C3\u30B7\u30E7\u30F3\
  \u3092\u958B\u59CB\u3067\u304D\u307E\u3059."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

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
