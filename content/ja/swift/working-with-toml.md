---
title:                "TOMLを扱う方法"
aliases:
- ja/swift/working-with-toml.md
date:                  2024-01-26T04:26:45.459149-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/working-with-toml.md"
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
