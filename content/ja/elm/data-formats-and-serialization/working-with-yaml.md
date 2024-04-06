---
date: 2024-01-19
description: "How to: (\u65B9\u6CD5) Elm\u3067YAML\u3092\u6271\u3046\u516C\u5F0F\u306E\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u5B58\u5728\u3057\u307E\u305B\u3093\u3002\u3057\
  \u304B\u3057\u3001JavaScript\u3068\u306E\u76F8\u4E92\u904B\u7528\u3092\u901A\u3058\
  \u3066YAML\u3092\u6271\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.922759-06:00'
model: unknown
summary: "(\u65B9\u6CD5) Elm\u3067YAML\u3092\u6271\u3046\u516C\u5F0F\u306E\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u306F\u5B58\u5728\u3057\u307E\u305B\u3093\u3002\u3057\u304B\u3057\
  \u3001JavaScript\u3068\u306E\u76F8\u4E92\u904B\u7528\u3092\u901A\u3058\u3066YAML\u3092\
  \u6271\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002"
title: "YAML\u3092\u6271\u3046"
weight: 41
---

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
