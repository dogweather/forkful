---
title:                "Elm: 「標準エラーに書き込む」"
simple_title:         "「標準エラーに書き込む」"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ？

あなたはプログラミングをするとき、しばしばバグやエラーに遭遇します。そのとき、コンソールにエラーメッセージが表示されることがあります。しかし、エラーメッセージをより詳しく知ることができれば、バグやエラーを修正するのに役立ちます。そのため、エラーメッセージを標準エラー出力に書き込むことは重要です。

## 使い方

標準エラー出力に書き込むには、ElmのDebugモジュールを使用します。以下のコードを参考にしてください。

```Elm
import Debug

main =
    let
        result = divide 10 0
    in
        case result of
            Ok value ->
                Debug.log "Success!" value

            Err error ->
                Debug.crash "Failed." error
```

このコードでは、10を0で割っているためエラーが発生します。しかし、それぞれのケースでエラーメッセージを標準エラー出力に書き込んでいます。このようにすることで、どのケースでエラーが発生したのかを知ることができます。

## 深堀り

エラーメッセージを標準エラー出力に書き込むことで、プログラマーはデバッグに役立つ情報を得ることができます。また、標準エラー出力に書き込むことで、エラーコードやスタックトレースなどの詳細な情報を表示することができます。

ただし、エラーメッセージを標準エラー出力に書き込む際には注意が必要です。標準エラー出力はコンソール上に表示されるため、ユーザーが見ることができます。そのため、個人情報やセンシティブな情報を標準エラー出力に書き込まないように注意しましょう。

## 併せて読みたい

- Elm Debugモジュールの公式ドキュメント：https://package.elm-lang.org/packages/elm/core/latest/Debug
- バグ修正に役立つ技術：https://www.toptal.com/qa/how-to-debug-your-bugs-like-a-pro
- エラーハンドリングのベストプラクティス：https://engineering.kablamo.com.au/posts/2019/understanding-error-handling-in-elm/