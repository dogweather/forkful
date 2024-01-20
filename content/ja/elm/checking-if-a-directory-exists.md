---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "Elm: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何 & なぜ?
ディレクトリが存在するかどうかを確認するというのは、指定したディレクトリの存在をプログラムでチェックすることです。プログラマーがこれを行う理由は、特定の操作を行う前に（ファイルの読み書きなど）対象ディレクトリの存在を確認し、エラーを防ぐためです。

## やり方:
Elm（現行バージョン）では残念ながら、ディレクトリが存在するかどうかを確認する直接的な方法は提供されていません。代わりにJavaScriptとの間で情報をやり取りすることで、この確認が可能になります。

```Elm
port module Main exposing (..)
import Html exposing (Html)
  
-- request to JavaScript to check directory
port directoryCheck : String -> Cmd msg
 
 -- get response from JavaScript
port resultOfCheck : (Bool -> msg) -> Sub msg
```
このコードはElmからJavaScriptにディレクトリの存在確認を依頼する部分です。

## ディープダイブ
ディレクトリの存在確認はOSが実行できる基本的なタスクですが、その実装はプログラミング言語とOSによって異なります。Elmは純粋な関数型言語で、副作用を避けるためにファイルシステムへの直接アクセスを提供していません。これは、クライアント側のWebプログラミングに焦点を当てているため、エラーハンドリングとの結びつきが緩いです。

しかし、JavaScriptを通じてこの機能を実現することができます。JavaScriptとの相互作用を可能にするパブリックポートを使用すると、ディレクトリの存在確認が可能になります。

## 関連リンク
1. ElmとJavaScriptの接続: [ElmとJavaScriptの相互作用](https://guide.elm-lang.org/interop/)
2. Elmについてさらに学ぶ: [Elm公式ガイド](https://guide.elm-lang.org/)