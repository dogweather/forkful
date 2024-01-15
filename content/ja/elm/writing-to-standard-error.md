---
title:                "標準エラーへの書き込み"
html_title:           "Elm: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

なぜコンソールに書き込むことが重要なのかを簡単に説明します。

コンソールに書き込むことはデバッグやエラートラッキングに役立ちます。エラーが発生したときに、むやみにコードを変更するのではなく、コンソールにメッセージを書き込んでデータフローを追跡することで、エラーの原因を特定することができます。

## How to

Elmでは、```Debug.log```関数を使用してコンソールに書き込むことができます。

例えば、Int型の変数をコンソールに書き出したい場合は、以下のように書きます。

```Elm
import Debug

numberOfApples = 5

Debug.log "There are" numberOfApples
```

出力結果は以下のようになります。

```
There are 5
```

## Deep Dive

```Debug.log```関数の書式は以下のようになります。

```
Debug.log : String -> a -> a
```

第1引数はコンソールに表示するメッセージの文字列、第2引数は任意の型のデータです。また、この関数は第2引数をそのまま返すので、例えば```Debug.log```をデータのパイプライン内で使用することができます。

さらに、```Debug.todo```関数を使用すると、特定の箇所が実行されたらコンソールに警告メッセージを表示することができます。

## See Also

こちらのリンクを参考にしてください。

[ElmのDebugモジュールのドキュメント](https://package.elm-lang.org/packages/elm/core/latest/Debug)