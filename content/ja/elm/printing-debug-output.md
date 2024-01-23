---
title:                "デバッグ出力を表示する"
date:                  2024-01-20T17:52:35.850118-07:00
model:                 gpt-4-1106-preview
simple_title:         "デバッグ出力を表示する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

プログラムでデバッグ出力を印刷することは、コードの実行中に何が起こっているかを理解するためです。これにより、プログラマーはバグを見つけて修正しやすくなります。

## How to: (やり方)

Elmでは、`Debug` モジュールを使ってデバッグ情報をコンソールに出力します。以下はその一例です。

```Elm
import Debug

main =
  let
    valueToInspect = "デバッグ出力するテキスト"
    _ = Debug.log "デバッグ情報" valueToInspect
  in
  -- これ以降でアプリケーションを定義します
```

これを実行すると、ブラウザのコンソールに以下のように表示されます。

```
デバッグ情報: "デバッグ出力するテキスト"
```

`Debug.log` の第一引数は、出力されるメッセージのラベル、第二引数は印刷したいデータです。

## Deep Dive (詳しい解説)

Elmの`Debug`モジュールはデバッグ専用で、最終的なアプリケーションには含めるべきではありません。歴史的には、デバッグ出力は多くの言語で行われており、コンソール出力はその一般的な方法です。

Elm 0.19では、`Debug`モジュールの一部の関数は本番環境のビルドで使うことができません。これは、デバッグコードが本番環境に侵入するのを防ぐためです。

`Debug.log`以外の方法としては、`Debug.todo`や`Debug.toString`などもありますが、基本的にデバッグ情報の出力は前述した方法に集約されます。

## See Also (関連情報)

- ElmのDebugモジュールに関する公式ドキュメント: https://package.elm-lang.org/packages/elm/core/latest/Debug
- コンソールデバッグについてのMozillaのドキュメント: https://developer.mozilla.org/en-US/docs/Learn/Common_questions/What_are_browser_developer_tools

これらのリソースを使って、Elmでのデバッグ出力についてさらに学んでください。
