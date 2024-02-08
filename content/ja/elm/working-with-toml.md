---
title:                "TOMLを扱う方法"
aliases:
- ja/elm/working-with-toml.md
date:                  2024-01-26T04:21:54.181579-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/working-with-toml.md"
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
