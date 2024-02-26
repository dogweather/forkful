---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:20.388428-07:00
description: "Swift\u3067JSON\u3092\u6271\u3046\u3068\u3044\u3046\u3053\u3068\u306F\
  \u3001\u30C7\u30FC\u30BF\u4EA4\u63DB\u306E\u305F\u3081\u306E\u8EFD\u91CF\u306A\u30C7\
  \u30FC\u30BF\u5F62\u5F0F\u3092\u6271\u3046\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001JSON\u3092\u30B5\u30FC\
  \u30D0\u30FC\u3068Web\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u9593\u3067\
  \u30C7\u30FC\u30BF\u3092\u4F1D\u9001\u3059\u308B\u305F\u3081\u306B\u4F7F\u7528\u3057\
  \u307E\u3059\u3002\u3053\u308C\u306F\u3001\u4EBA\u9593\u3068\u6A5F\u68B0\u306E\u4E21\
  \u65B9\u306B\u3068\u3063\u3066\u8AAD\u307F\u3084\u3059\u304F\u3001\u89E3\u6790\u3057\
  \u3084\u3059\u3044\u305F\u3081\u3067\u3059\u3002"
lastmod: '2024-02-25T18:49:40.593309-07:00'
model: gpt-4-0125-preview
summary: "Swift\u3067JSON\u3092\u6271\u3046\u3068\u3044\u3046\u3053\u3068\u306F\u3001\
  \u30C7\u30FC\u30BF\u4EA4\u63DB\u306E\u305F\u3081\u306E\u8EFD\u91CF\u306A\u30C7\u30FC\
  \u30BF\u5F62\u5F0F\u3092\u6271\u3046\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001JSON\u3092\u30B5\u30FC\u30D0\
  \u30FC\u3068Web\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u9593\u3067\u30C7\
  \u30FC\u30BF\u3092\u4F1D\u9001\u3059\u308B\u305F\u3081\u306B\u4F7F\u7528\u3057\u307E\
  \u3059\u3002\u3053\u308C\u306F\u3001\u4EBA\u9593\u3068\u6A5F\u68B0\u306E\u4E21\u65B9\
  \u306B\u3068\u3063\u3066\u8AAD\u307F\u3084\u3059\u304F\u3001\u89E3\u6790\u3057\u3084\
  \u3059\u3044\u305F\u3081\u3067\u3059\u3002"
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

SwiftでJSONを扱うということは、データ交換のための軽量なデータ形式を扱うことを意味します。プログラマーは、JSONをサーバーとWebアプリケーション間でデータを伝送するために使用します。これは、人間と機械の両方にとって読みやすく、解析しやすいためです。

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
