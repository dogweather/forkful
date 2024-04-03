---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:20.388428-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Swift\u3067\u306F\u3001\
  `Codable`\u30D7\u30ED\u30C8\u30B3\u30EB\u3092\u4F7F\u3063\u3066JSON\u306E\u89E3\u6790\
  \u3092\u76F4\u611F\u7684\u306B\u884C\u3048\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\
  JSON\u3092Swift\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u30C7\u30B3\u30FC\u30C9\
  \u3059\u308B\u65B9\u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.646880-06:00'
model: gpt-4-0125-preview
summary: "Swift\u3067\u306F\u3001`Codable`\u30D7\u30ED\u30C8\u30B3\u30EB\u3092\u4F7F\
  \u3063\u3066JSON\u306E\u89E3\u6790\u3092\u76F4\u611F\u7684\u306B\u884C\u3048\u307E\
  \u3059\u3002\u4EE5\u4E0B\u306F\u3001JSON\u3092Swift\u30AA\u30D6\u30B8\u30A7\u30AF\
  \u30C8\u306B\u30C7\u30B3\u30FC\u30C9\u3059\u308B\u65B9\u6CD5\u3067\u3059\uFF1A."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## どのようにして：
Swiftでは、`Codable`プロトコルを使ってJSONの解析を直感的に行えます。以下は、JSONをSwiftオブジェクトにデコードする方法です：

```Swift
import Foundation

// Codableに準拠するモデルを定義
struct User: Codable {
    var name: String
    var age: Int
}

// JSON文字列
let jsonString = """
{
    "name": "John Doe",
    "age": 30
}
"""

// JSON文字列をDataに変換
if let jsonData = jsonString.data(using: .utf8) {
    // JSONデータをUserオブジェクトにデコード
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("名前: \(user.name), 年齢: \(user.age)")
    } catch {
        print("JSONのデコードエラー: \(error)")
    }
}
```

サンプル出力：
```
名前: John Doe, 年齢: 30
```

## 深掘り
JSON（JavaScriptオブジェクト表記）は、ダグラス・クロックフォードが指定して以来、2000年代初頭から広く普及しました。JSONは、よりシンプルな構文と優れたパフォーマンスのために、多くのユースケースでXMLを置き換えました。Swiftの`Codable`はJSONに対する主なツールですが、Codable非準拠タイプを扱う場合の代替手段として`JSONSerialization`などが存在します。内部では、`Codable`は下層のパーシングを抽象化し、シリアル化/デシリアル化をシームレスにします。

## 関連項目
- 公式のSwiftブログでJSONとSwiftについてもっと探求する：[Swift.org](https://swift.org/blog/)
- `Codable`のドキュメントをチェックアウトする：[Swift Codable](https://developer.apple.com/documentation/swift/codable)
- 複雑なJSON構造には、[GitHub](https://github.com/SwiftyJSON/SwiftyJSON)で入手可能なSwiftyJSONなどのサードパーティライブラリを検討する。
