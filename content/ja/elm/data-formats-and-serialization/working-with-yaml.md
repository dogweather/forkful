---
title:                "YAMLを扱う"
aliases:
- /ja/elm/working-with-yaml/
date:                  2024-01-19
simple_title:         "YAMLを扱う"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
YAMLは人間可読なデータシリアライゼーション形式です。構成ファイルやデータ交換で使われ、その明瞭さからプログラマーに好まれています。

## How to: (方法)
ElmでYAMLを扱う公式のライブラリは存在しません。しかし、JavaScriptとの相互運用を通じてYAMLを扱うことができます。

```Elm
port module Main exposing (..)

-- ElmからJavaScriptにデータを送るポート
port toJs : String -> Cmd msg

-- JavaScriptからElmにデータを受け取るポート
port fromJs : (String -> msg) -> Sub msg

-- YAML文字列をJavaScriptに送ってパースを依頼する
parseYaml : String -> Cmd msg
parseYaml yamlString =
    toJs yamlString
```

JavaScript側でパースを行うコードは次の通りです:

```javascript
// portsを使ってElmと通信する
app.ports.toJs.subscribe(function(yamlString) {
    try {
        var result = YAML.parse(yamlString); // パース
        app.ports.fromJs.send(JSON.stringify(result)); // 結果をElmに送信
    } catch(error) {
        app.ports.fromJs.send(JSON.stringify({ error: error.message })); // エラーをElmに送信
    }
});
```

Elmでの受け取り例は以下の通りです。

```Elm
type Msg
    = ReceiveParsedData String

subscriptions : Model -> Sub Msg
subscriptions _ =
    fromJs ReceiveParsedData
```

## Deep Dive (深いダイブ)
YAMLはYAML Ain't Markup Languageの略で、設定ファイルなどに使われることが多いです。JSONの代わりとしても使われており、人が読み書きしやすい形式として設計されています。Elmにはネイティブサポートはないですが、JavaScriptと組み合わせることで簡単に扱うことが可能です。

## See Also (関連リンク)
- YAMLの公式サイト: https://yaml.org
- ElmのJSONの扱い方: https://package.elm-lang.org/packages/elm/json/latest/
- Elmのポートについて: https://guide.elm-lang.org/interop/ports.html
