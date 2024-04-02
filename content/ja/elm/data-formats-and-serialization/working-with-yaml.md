---
date: 2024-01-19
description: "YAML\u306F\u4EBA\u9593\u53EF\u8AAD\u306A\u30C7\u30FC\u30BF\u30B7\u30EA\
  \u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u5F62\u5F0F\u3067\u3059\u3002\u69CB\
  \u6210\u30D5\u30A1\u30A4\u30EB\u3084\u30C7\u30FC\u30BF\u4EA4\u63DB\u3067\u4F7F\u308F\
  \u308C\u3001\u305D\u306E\u660E\u77AD\u3055\u304B\u3089\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306B\u597D\u307E\u308C\u3066\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.035609-06:00'
model: unknown
summary: "YAML\u306F\u4EBA\u9593\u53EF\u8AAD\u306A\u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\
  \u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u5F62\u5F0F\u3067\u3059\u3002\u69CB\u6210\
  \u30D5\u30A1\u30A4\u30EB\u3084\u30C7\u30FC\u30BF\u4EA4\u63DB\u3067\u4F7F\u308F\u308C\
  \u3001\u305D\u306E\u660E\u77AD\u3055\u304B\u3089\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306B\u597D\u307E\u308C\u3066\u3044\u307E\u3059\u3002"
title: "YAML\u3092\u6271\u3046"
weight: 41
---

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
