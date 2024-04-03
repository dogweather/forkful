---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:03.476452-07:00
description: "YAML\u306F\u3001\"YAML Ain't Markup\u2026"
lastmod: '2024-03-13T22:44:42.645369-06:00'
model: gpt-4-0125-preview
summary: "YAML\u306F\u3001\"YAML Ain't Markup Language\"\u306E\u7565\u3067\u3001\u3059\
  \u3079\u3066\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u306B\u5BFE\
  \u3059\u308B\u4EBA\u9593\u306B\u512A\u3057\u3044\u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\
  \u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u6A19\u6E96\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u305D\u306E\u8AAD\u307F\u3084\u3059\u3055\u304C\
  XML\u3084JSON\u306A\u3069\u306E\u4ED6\u306E\u30C7\u30FC\u30BF\u5F62\u5F0F\u306B\u6BD4\
  \u3079\u3066\u30D7\u30EC\u30FC\u30F3\u306A\u82F1\u8A9E\u306B\u3088\u308A\u8FD1\u3044\
  \u305F\u3081\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3001\u30D7\u30ED\u30BB\u30B9\
  \u9593\u30E1\u30C3\u30BB\u30FC\u30B8\u30F3\u30B0\u3001\u30C7\u30FC\u30BF\u30B9\u30C8\
  \u30EC\u30FC\u30B8\u7528\u306B\u4F7F\u7528\u3057\u3066\u3044\u307E\u3059\u3002\u3053\
  \u308C\u306B\u3088\u308A\u7406\u89E3\u3057\u3084\u3059\u304F\u3001\u66F8\u304D\u3084\
  \u3059\u304F\u306A\u3063\u3066\u3044\u307E\u3059\u3002."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 方法:
SwiftはYAMLの解析とシリアライゼーションのための組み込みサポートを含んでいません。これにより、サードパーティーのライブラリを使用する必要があります。人気の選択肢は`Yams`で、SwiftでYAMLを扱うためのライブラリです。

まず、`Yams`をプロジェクトに追加する必要があります。Swiftパッケージマネージャーを使用している場合は、`Package.swift`ファイルに依存性として追加できます：

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

### SwiftへのYAMLパーシング
簡単なアプリケーションのための以下のYAML設定を持っていると仮定します：

```yaml
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
```

以下は、`Yams`を使ってこのYAML文字列をSwiftで解析する方法です：

```swift
import Yams

let yamlString = """
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
        // 解析されたデータへの例示的なアクセス
        if let name = data["name"] as? String {
            print("アプリ名: \(name)")
        }
    }
} catch {
    print("YAMLの解析エラー: \(error)")
}
```

サンプル出力:

```
["name": MyApp, "version": 1.0, "environment": "development", "features": ["login", "notifications"]]
アプリ名: MyApp
```

### SwiftオブジェクトをYAMLにシリアライズする
SwiftオブジェクトをYAML文字列に戻すことも、`Yams`を使えば簡単です。シリアライズされる必要がある同じデータ構造を持っていることを想定してください：

```swift
let appInfo = [
    "name": "MyApp",
    "version": 1.0,
    "environment": "development",
    "features": ["login", "notifications"]
] as [String : Any]

do {
    let yamlString = try Yams.dump(object: appInfo)
    print(yamlString)
} catch {
    print("YAMLへのシリアライズ エラー: \(error)")
}
```

これにより、YAML形式の文字列が生成されます：

```yaml
environment: development
features:
  - login
  - notifications
name: MyApp
version: 1.0
```

これらの例は、SwiftアプリケーションでYAMLを扱うための基本的な操作を示しています。YAMLは人間の読みやすさと使いやすさで優れていますが、データシリアライゼーション形式を選択する際には、パフォーマンスや複雑さに関して特に、アプリケーションの具体的なニーズを常に考慮してください。
