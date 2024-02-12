---
title:                "標準エラーへの書き込み"
aliases:
- /ja/swift/writing-to-standard-error/
date:                  2024-02-03T19:34:59.737492-07:00
model:                 gpt-4-0125-preview
simple_title:         "標準エラーへの書き込み"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

標準エラー（stderr）への書き込みは、プログラムのエラーメッセージや診断出力を、標準出力（stdout）とは別のストリームに向けることを意味します。これは、標準出力を混乱させることなく、デバッグやエラーログを行う上で重要であり、開発者とユーザー双方がプログラムの状態や問題を理解するのを容易にします。

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
