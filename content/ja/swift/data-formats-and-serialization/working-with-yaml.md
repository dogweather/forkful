---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:03.476452-07:00
description: "\u65B9\u6CD5: Swift\u306FYAML\u306E\u89E3\u6790\u3068\u30B7\u30EA\u30A2\
  \u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u306E\u305F\u3081\u306E\u7D44\u307F\u8FBC\
  \u307F\u30B5\u30DD\u30FC\u30C8\u3092\u542B\u3093\u3067\u3044\u307E\u305B\u3093\u3002\
  \u3053\u308C\u306B\u3088\u308A\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30FC\
  \u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3059\u308B\u5FC5\u8981\u304C\
  \u3042\u308A\u307E\u3059\u3002\u4EBA\u6C17\u306E\u9078\u629E\u80A2\u306F`Yams`\u3067\
  \u3001Swift\u3067YAML\u3092\u6271\u3046\u305F\u3081\u306E\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3067\u3059\u3002\u2026"
lastmod: '2024-03-13T22:44:42.645369-06:00'
model: gpt-4-0125-preview
summary: "Swift\u306FYAML\u306E\u89E3\u6790\u3068\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\
  \u30FC\u30B7\u30E7\u30F3\u306E\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u30B5\u30DD\
  \u30FC\u30C8\u3092\u542B\u3093\u3067\u3044\u307E\u305B\u3093\u3002\u3053\u308C\u306B\
  \u3088\u308A\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30FC\u306E\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\
  \u3059\u3002\u4EBA\u6C17\u306E\u9078\u629E\u80A2\u306F`Yams`\u3067\u3001Swift\u3067\
  YAML\u3092\u6271\u3046\u305F\u3081\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3067\u3059\
  ."
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
