---
date: 2024-01-26 04:26:45.459149-07:00
description: "\u65B9\u6CD5 \u307E\u305A\u3001TOML\u30D1\u30FC\u30B5\u30FC\u304C\u5FC5\
  \u8981\u3067\u3059\u3002Swift\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\u3082\u306E\
  \u304C\u306A\u3044\u305F\u3081\u3001`TOMLDecoder`\u3092\u4F7F\u7528\u3057\u307E\u3057\
  \u3087\u3046\u3002Swift Package Manager\u3092\u901A\u3058\u3066\u30A4\u30F3\u30B9\
  \u30C8\u30FC\u30EB\u3057\u3001\u305D\u306E\u5F8C\u7C21\u5358\u306BTOML\u3092\u30B7\
  \u30EA\u30A2\u30E9\u30A4\u30BA\u304A\u3088\u3073\u30C7\u30B7\u30EA\u30A2\u30E9\u30A4\
  \u30BA\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.649364-06:00'
model: gpt-4-0125-preview
summary: "\u307E\u305A\u3001TOML\u30D1\u30FC\u30B5\u30FC\u304C\u5FC5\u8981\u3067\u3059\
  \u3002Swift\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\u3082\u306E\u304C\u306A\u3044\
  \u305F\u3081\u3001`TOMLDecoder`\u3092\u4F7F\u7528\u3057\u307E\u3057\u3087\u3046\u3002\
  Swift Package Manager\u3092\u901A\u3058\u3066\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\
  \u3057\u3001\u305D\u306E\u5F8C\u7C21\u5358\u306BTOML\u3092\u30B7\u30EA\u30A2\u30E9\
  \u30A4\u30BA\u304A\u3088\u3073\u30C7\u30B7\u30EA\u30A2\u30E9\u30A4\u30BA\u3057\u307E\
  \u3059."
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

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
