---
date: 2024-01-26 04:21:54.181579-07:00
description: "TOML\u306F\u3001Tom's Obvious, Minimal Language\u306E\u7565\u3067\u3001\
  \u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u8A00\
  \u8A9E\u3067\u3059\u3002Elm\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u305D\
  \u308C\u304C\u4EBA\u9593\u306B\u3068\u3063\u3066\u8AAD\u307F\u3084\u3059\u304F\u3001\
  \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u5FC5\u8981\u306A\u30AD\u30FC\
  \u5024\u306E\u30DA\u30A2\u306B\u304D\u308C\u3044\u306B\u30DE\u30C3\u30D4\u30F3\u30B0\
  \u3059\u308B\u305F\u3081\u3001\u8A2D\u5B9A\u30C7\u30FC\u30BF\u306E\u7BA1\u7406\u306B\
  \u4F7F\u7528\u3057\u3066\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.038845-06:00'
model: gpt-4-0125-preview
summary: "TOML\u306F\u3001Tom's Obvious, Minimal Language\u306E\u7565\u3067\u3001\u30C7\
  \u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u8A00\u8A9E\
  \u3067\u3059\u3002Elm\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u305D\u308C\
  \u304C\u4EBA\u9593\u306B\u3068\u3063\u3066\u8AAD\u307F\u3084\u3059\u304F\u3001\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u5FC5\u8981\u306A\u30AD\u30FC\u5024\
  \u306E\u30DA\u30A2\u306B\u304D\u308C\u3044\u306B\u30DE\u30C3\u30D4\u30F3\u30B0\u3059\
  \u308B\u305F\u3081\u3001\u8A2D\u5B9A\u30C7\u30FC\u30BF\u306E\u7BA1\u7406\u306B\u4F7F\
  \u7528\u3057\u3066\u3044\u307E\u3059\u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
---

{{< edit_this_page >}}

## 何となく？
TOMLは、Tom's Obvious, Minimal Languageの略で、データシリアライゼーション言語です。Elmプログラマーは、それが人間にとって読みやすく、アプリケーションに必要なキー値のペアにきれいにマッピングするため、設定データの管理に使用しています。

## 方法：
Elmには組み込みのTOMLパーサーはありませんが、JavaScriptとの相互運用を行うか、コミュニティパッケージを使用できます。これは、仮定の`elm-toml`パッケージを使用してTOMLを解析する方法です：

```elm
import Toml

configToml : String
configToml =
    """
    [server]
    port = 8080
    """

parseResult : Result Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

特定の値をデコードする場合：

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

`port`のサンプル出力は、デコードが成功した場合`Ok 8080`かもしれません。

## 深く掘り下げ
TOMLは、GitHubの共同創設者であるTom Preston-Wernerによって、設定ファイル用のシンプルな言語として作られました。TOMLはYAMLやJSONと競合し、両方の世界のベストを目指した構文を持ち、人間が読み書きしやすいことに焦点を当てています。

ElmでTOMLを扱うには、通常、JavaScriptとの相互運用を通じて行う必要があり、これは少し面倒かもしれません。幸いなことに、Elmコミュニティは資源豊富であり、いくつかのサードパーティパッケージが存在します。仮定の`elm-toml`パッケージは、Elmの`Port`を使用してJavaScriptのTOMLパーサーと話をするか、直接Elmで解析を実装するでしょう。

Elmの主な障害は、すべてを静的に型付けすることであり、TOML内の異なるデータ構造を扱うためにカスタムデコーダを書く必要があるため、少し冗長になる可能性がありますが、安全性を追加します。

## 関連情報
TOML自体の仕様と詳細については、[TOML](https://toml.io)をご覧ください。
ElmとJavaScriptの相互運用に取り組む場合は、公式ガイドから始めてください：[Elm Ports](https://guide.elm-lang.org/interop/ports.html)。
コミュニティパッケージを探すか、貢献するには、[Elm Packages](https://package.elm-lang.org/)を参照してください。
