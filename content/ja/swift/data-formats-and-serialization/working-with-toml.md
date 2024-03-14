---
date: 2024-01-26 04:26:45.459149-07:00
description: "TOML\uFF08Tom's Obvious, Minimal Language\uFF09\u306F\u3001\u305D\u306E\
  \u660E\u5FEB\u306A\u30BB\u30DE\u30F3\u30C6\u30A3\u30AF\u30B9\u306E\u304A\u304B\u3052\
  \u3067\u8AAD\u307F\u3084\u3059\u3044\u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\
  \u30BC\u30FC\u30B7\u30E7\u30F3\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4EBA\u9593\u306B\u3088\u308B\u53EF\
  \u8AAD\u6027\u3068\u6A5F\u68B0\u306B\u3088\u308B\u7C21\u5358\u306A\u89E3\u6790\u304C\
  \u9375\u3068\u306A\u308B\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u306BTOML\u3092\u4F7F\
  \u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.649364-06:00'
model: gpt-4-0125-preview
summary: "TOML\uFF08Tom's Obvious, Minimal Language\uFF09\u306F\u3001\u305D\u306E\u660E\
  \u5FEB\u306A\u30BB\u30DE\u30F3\u30C6\u30A3\u30AF\u30B9\u306E\u304A\u304B\u3052\u3067\
  \u8AAD\u307F\u3084\u3059\u3044\u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\
  \u30FC\u30B7\u30E7\u30F3\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4EBA\u9593\u306B\u3088\u308B\u53EF\u8AAD\
  \u6027\u3068\u6A5F\u68B0\u306B\u3088\u308B\u7C21\u5358\u306A\u89E3\u6790\u304C\u9375\
  \u3068\u306A\u308B\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u306BTOML\u3092\u4F7F\u7528\
  \u3057\u307E\u3059\u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
---

{{< edit_this_page >}}

## はじめに
TOML（Tom's Obvious, Minimal Language）は、その明快なセマンティクスのおかげで読みやすいデータシリアライゼーションフォーマットです。プログラマーは、人間による可読性と機械による簡単な解析が鍵となる設定ファイルにTOMLを使用します。

## 方法
まず、TOMLパーサーが必要です。Swiftには組み込みのものがないため、`TOMLDecoder`を使用しましょう。Swift Package Managerを通じてインストールし、その後簡単にTOMLをシリアライズおよびデシリアライズします。

```Swift
import TOMLDecoder

let tomlString = """
title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("Title: \(config.title), Owner: \(config.owner.name), DOB: \(config.owner.dob)")
    } catch {
        print("Error parsing TOML: \(error)")
    }
}
```

このコードの出力：
```
Title: TOML Example, Owner: Tom Preston-Werner, DOB: 1979-05-27 07:32:00 +0000
```

## 深掘り
TOMLは、GitHubの共同創設者であるTom Preston-Wernerによってデザインされ、JSONやYAMLのようなフォーマットよりも人間に優しい代替品として提案されました。それは明瞭さを目指し、人間や機械による誤解の可能性を減らします。代替品に関しては、YAMLとJSONがよく知られており、YAMLは人間の可読性に偏っており、JSONはよりシンプルな機械向けのオプションです。SwiftでTOMLを扱う場合、ネイティブのパーサーはありません。しかし、`TOMLDecoder`のようなサードパーティのライブラリは、特にSwift 4で導入された`Codable`プロトコルを介してTOML文字列とSwiftの型の間の簡単な変換を促進します。

## 参照
- TOML標準：https://toml.io
- `TOMLDecoder`のGitHub：https://github.com/dduan/TOMLDecoder
- `Codable`についてのSwiftドキュメント：https://developer.apple.com/documentation/swift/codable
- データシリアライゼーションフォーマットの比較：https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
