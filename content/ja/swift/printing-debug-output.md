---
title:                "デバッグ出力の印刷"
html_title:           "Fish Shell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何となぜ?

デバッグ出力とは、コードが正常に動作しているかをチェックする方法の一つです。プログラマーがそれを実施する理由は、問題を特定し、解決するためです。

## 使い方:

Swiftの基本的なデバック出力は、以下のような `print` 文を使用することで行うことができます。

```Swift
let name = "Yamada"
print("Hello, \(name)")
```

これはコンソールに `"Hello, Yamada"` と映し出します。

さらに、デバッグ専用の出力には `debugPrint` を使用します。これはコンプレックスなデータ型に特に役立ちます。

```Swift
let array = ["Yamada", "Suzuki", "Tanaka"]
debugPrint(array)
```

この結果、コンソールには `["Yamada", "Suzuki", "Tanaka"]` が表示されます。

## Deep Dive

デバッグ出力はコンピュータープログラミングの初期から存在し、Swiftはこれを独自の方法で実装しました。

Swiftにはデバッグ出力のために`print`と`debugPrint`の2つの主要な関数がありますが、これらは各々異なる目的を果たします。

`print`は基本的なデバッグ出力に使用され、文字列を明快に表示します。一方`debugPrint`はより詳細な出力を提供し、特に複雑なデータ型をデバッグする際に役立ちます。

また、フレームワークやライブラリを使用してログの出力をカスタマイズする方法もあります。例えば、`OSLog`や`SwiftLog`などが存在します。

## 関連するリソース

以下は、デバッグ出力に関する追加的な情報へのリンクです。

1. Appleの[公式ドキュメント](https://developer.apple.com/documentation/swift/1541053-print)
2. Swiftのデバッグ出力に関する[ブログ記事](https://nshipster.com/customplaygrounddisplayconvertible/)
3. Swiftでのロギングのベストプラクティスについての[解説記事](https://www.raywenderlich.com/6059-logging-in-swift)