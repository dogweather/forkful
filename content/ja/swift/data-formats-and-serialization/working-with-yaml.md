---
aliases:
- /ja/swift/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:03.476452-07:00
description: "YAML\u306F\u3001\"YAML Ain't Markup\u2026"
lastmod: 2024-02-18 23:08:55.248859
model: gpt-4-0125-preview
summary: "YAML\u306F\u3001\"YAML Ain't Markup\u2026"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？
YAMLは、"YAML Ain't Markup Language"の略で、すべてのプログラミング言語に対する人間に優しいデータシリアライゼーション標準です。プログラマーは、その読みやすさがXMLやJSONなどの他のデータ形式に比べてプレーンな英語により近いため、設定ファイル、プロセス間メッセージング、データストレージ用に使用しています。これにより理解しやすく、書きやすくなっています。

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
