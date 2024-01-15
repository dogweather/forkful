---
title:                "デバッグ出力を印刷する"
html_title:           "Elm: デバッグ出力を印刷する"
simple_title:         "デバッグ出力を印刷する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

デバッグ出力を表示することに関わって最も重要なのは、コードを理解するためです。デバッグ出力を使用することにより、開発者はコードの実行中に何が起こっているのかをより詳細に把握することができます。

## 方法

デバッグ出力を表示するには、 `Debug.log` 関数を使います。以下は、 `Debug.log` 関数を使用して値を出力する例です。

```Elm
import Debug

main =
  let
    name = "John"
    age = 25
  in
    Debug.log "Name: " name
    Debug.log "Age: " (toString age)
```

上記のコードでは、 `Debug.log` 関数を使用して`name`と`age`の値を出力しています。出力はデバッガーに表示されるため、実行中に変数の値を確認することができます。

また、複雑なデータ構造の値を出力する場合は、 `Debug.toString` 関数を使用することもできます。

```Elm
import Debug

type alias Person =
  { name: String
  , age: Int
  , address: String
  }

person = Person "John" 25 "Tokyo"

Debug.log "Person: " (Debug.toString person)
```

出力結果は以下のようになります。

```elm
Person: 
    { address = "Tokyo"
    , age = 25
    , name = "John"
    }
```

## 深堀り

`Debug.log` 関数を使用する際に注意することがあります。デバッグ出力は必ずしも正しい値を反映しない場合があり、最終的なプログラムの動作に影響しないこともあります。コードの最適化や最適箇所の特定には、 `Debug.log` を使用することが役立つ場合がありますが、本番環境では避けるべきです。

## 参考リンク

- [Official guide to Debugging in Elm](https://guide.elm-lang.org/debugging/debugging.html)
- [Debug module documentation](https://package.elm-lang.org/packages/elm/core/latest/Debug)'