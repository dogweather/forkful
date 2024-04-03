---
date: 2024-01-26 04:21:54.181579-07:00
description: "\u65B9\u6CD5\uFF1A Elm\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306ETOML\u30D1\
  \u30FC\u30B5\u30FC\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001JavaScript\u3068\
  \u306E\u76F8\u4E92\u904B\u7528\u3092\u884C\u3046\u304B\u3001\u30B3\u30DF\u30E5\u30CB\
  \u30C6\u30A3\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\
  \u3002\u3053\u308C\u306F\u3001\u4EEE\u5B9A\u306E`elm-toml`\u30D1\u30C3\u30B1\u30FC\
  \u30B8\u3092\u4F7F\u7528\u3057\u3066TOML\u3092\u89E3\u6790\u3059\u308B\u65B9\u6CD5\
  \u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.038845-06:00'
model: gpt-4-0125-preview
summary: "Elm\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306ETOML\u30D1\u30FC\u30B5\u30FC\
  \u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001JavaScript\u3068\u306E\u76F8\u4E92\
  \u904B\u7528\u3092\u884C\u3046\u304B\u3001\u30B3\u30DF\u30E5\u30CB\u30C6\u30A3\u30D1\
  \u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002\u3053\u308C\
  \u306F\u3001\u4EEE\u5B9A\u306E`elm-toml`\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\
  \u7528\u3057\u3066TOML\u3092\u89E3\u6790\u3059\u308B\u65B9\u6CD5\u3067\u3059\uFF1A\
  ."
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

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
