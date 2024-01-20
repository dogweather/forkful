---
title:                "標準エラーへの書き込み"
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
### 何となぜ？
エラーメッセージは標準エラーに書き込む。プログラムのメイン出力とエラーを分けるためだ。

## How to:
### 方法:
Elmはブラウザで動くので、直接的な標準エラーへの書き込みはできません。代わりにJavaScriptと連携してコンソールにログを出力します。

```Elm
port module Main exposing (..)

-- Elm コードでポートを定義
port error : String -> Cmd msg

-- エラーメッセージを送る関数
reportError : String -> Cmd msg
reportError message =
    error message

-- サンプルのメイン関数
main : Program () Never ()
main =
    Html.program
        { init = () ! [ reportError "何かエラーが発生しました" ]
        , view = \_ -> Html.text ""
        , update = \_ _ -> ((), Cmd.none)
        , subscriptions = \_ -> Sub.none
        }
```

JavaScript側:
```javascript
// Elmアプリ起動
var app = Elm.Main.init({
  node: document.getElementById('elm')
});

// ポートの購読
app.ports.error.subscribe(function(message) {
  console.error("Elmからのエラー: ", message);
});
```

サンプル出力:
```
Elmからのエラー: 何かエラーが発生しました
```

## Deep Dive
### 詳細情報:
元々、コンピュータシステムでは標準出力(STDOUT)と標準エラー(STDERR)を区別していました。エラーメッセージをSTDERRに書き込むのは、重要な情報を見逃さないようにするためです。Elmが直接サポートしていないため、外部のJavaScriptが必要です。

## See Also
### 関連情報:
- Elm公式ドキュメント: https://elm-lang.org/docs
- Elmポートについてのガイド: https://guide.elm-lang.org/interop/ports.html
- JavaScriptの`Console`: https://developer.mozilla.org/en-US/docs/Web/API/Console/error