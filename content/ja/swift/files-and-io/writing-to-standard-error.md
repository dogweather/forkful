---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:59.737492-07:00
description: "\u65B9\u6CD5\uFF1A Swift\u3067\u6A19\u6E96\u30A8\u30E9\u30FC\u306B\u66F8\
  \u304D\u8FBC\u3080\u306B\u306F\u3001stderr\u3078\u306E\u76F4\u63A5\u30A2\u30AF\u30BB\
  \u30B9\u306B `FileHandle` \u30AF\u30E9\u30B9\u3092\u4F7F\u7528\u3057\u307E\u3059\
  \u3002\u4EE5\u4E0B\u306F\u7C21\u5358\u306A\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.433917-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

## 方法：
Swiftで標準エラーに書き込むには、stderrへの直接アクセスに `FileHandle` クラスを使用します。以下は簡単な例です：

```swift
import Foundation

// メッセージの定義
let errorMessage = "An error occurred.\n"

// メッセージをデータに変換
if let data = errorMessage.data(using: .utf8) {
    // エラーメッセージをstderrに書き込む
    FileHandle.standardError.write(data)
}
```

stderrへの出力（通常はコンソールやターミナルで確認されます）：
```
An error occurred.
```

より複雑なログ処理が必要な場合や、外部ライブラリを使用する場合、**SwiftLog** のようなサードパーティのライブラリの使用を検討するかもしれません。**SwiftLog** はデフォルトでは直接stderrに書き込みませんが、カスタムログバックエンドを実装することでこれを実現できます。stderrに書き込むカスタムログハンドラを定義する簡略化された例を以下に示します：

まず、`Package.swift` に **SwiftLog** をプロジェクトの依存関係として追加します：
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "YourPackageName",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "YourTargetName",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
            ]),
    ]
)
```

次に、stderrに書き込むカスタムログハンドラを実装します：

```swift
import Logging
import Foundation

struct StderrLogHandler: LogHandler {
    let label: String
    
    var logLevel: Logger.Level = .info
    
    func log(level: Logger.Level, message: Logger.Message, metadata: Logger.Metadata?, source: String, file: String, function: String, line: UInt) {
        let output = "\(message)\n"
        if let data = output.data(using: .utf8) {
            FileHandle.standardError.write(data)
        }
    }
    
    subscript(metadataKey metadataKey: String) -> Logger.Metadata.Value? {
        get { return nil }
        set(newValue) { }
    }
    
    var metadata: Logger.Metadata {
        get { return [:] }
        set(newMetadata) { }
    }
}

// 使用法
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.example.yourapp")

logger.error("This is an error message")
```

stderrへの出力：
```
This is an error message
```

このカスタムハンドラにより、SwiftLogのエラーメッセージを直接標準エラーにルーティングし、アプリケーションが生成する他のログメッセージとシームレスに統合できます。
