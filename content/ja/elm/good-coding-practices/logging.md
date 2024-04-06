---
date: 2024-01-26 01:02:25.796418-07:00
description: "\u65B9\u6CD5 Elm\u306E\u30A2\u30FC\u30AD\u30C6\u30AF\u30C1\u30E3\u306F\
  \u3001\u7BB1\u304B\u3089\u51FA\u3057\u3066\u3059\u3050\u306B\u4F7F\u3048\u308B\u30ED\
  \u30B0\u306E\u3088\u3046\u306A\u526F\u4F5C\u7528\u3092\u30B5\u30DD\u30FC\u30C8\u3057\
  \u3066\u3044\u307E\u305B\u3093 -\u2026"
lastmod: '2024-04-05T21:53:42.902797-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

## 方法
Elmのアーキテクチャは、箱から出してすぐに使えるログのような副作用をサポートしていません - それらはコマンドを通して扱われ、それはあなたのアプリケーションのアーキテクチャの一部です。教育目的で、ポートを通じてJavaScriptへメッセージを送信することによりログをシミュレートする方法を見てみましょう。

まず、ポートモジュールを定義します：

```Elm
port module Logger exposing (..)

-- JavaScriptにログを送出するためのポートを定義する
port log : String -> Cmd msg
```

`Main.elm`内で、ログメッセージを送出するために`log`ポートを使用します：

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- ここでモデルにいくつかの更新を行う
            ( updatedModel, log "AnEvent occurred." )

        AnotherEvent ->
            -- ここでモデルに他の更新を行う
            ( anotherUpdatedModel, log "AnotherEvent occurred." )
```

JavaScript側では、入ってくるログメッセージを処理するために`log`ポートを購読します：

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

これによりJavaScriptコンソールでのサンプル出力は以下のようになります：

```
AnEvent occurred.
AnotherEvent occurred.
```

## 深掘り
伝統的に、PythonやJavaなどの言語では、ログはログライブラリを使用して行われ、debug、info、warning、error、criticalなどの様々なレベルでメッセージをログに記録するための直接的なAPIを提供します。

純粋性と不変性に焦点を当てているElmでは、この種の直接的なログを提供していません。なぜなら、IOや副作用はElmアーキテクチャを通じて明確に管理されているからです。

Elmで完全な機能を持つログが必要な場合、通常は外部のJavaScriptツールに頼ります。上述のように、ポートはこれらのツールへの橋渡しを行います。Debugモジュールも別の選択肢ですが、これは開発用途のみを意図しており、本番環境でのログのためには使用されません。

ポートに加えて、プログラマーはElmのコンパイラーメッセージやランタイムデバッグ施設、例えば`Debug.log`をしばしば使用します。これは、値をトレースするためにコードに挿入することができ、次のように式をラップしてその出力をコンソールにログします：

```Elm
view model =
    Debug.log "Model Debug" model
    -- ここでビューコードが続きます
```

これもまた、本番使用を意図していません。elm-loggerのようなツールは、開発用途よりも本番環境での使用のためにポート上のいくつかの抽象化を提供しますが、これらも開発用途での使用を意図しています。

## 関連項目
- Elmのポート: https://guide.elm-lang.org/interop/ports.html
- Elmの`Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Elmとログに関する議論：https://discourse.elm-lang.org/t/elm-and-logging/546
- JavaScriptコンソールAPI：https://developer.mozilla.org/en-US/docs/Web/API/Console
- elm-loggerパッケージ：https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
