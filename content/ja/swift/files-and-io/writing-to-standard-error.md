---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:59.737492-07:00
description: "\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\u306E\u66F8\u304D\
  \u8FBC\u307F\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u30A8\u30E9\u30FC\u30E1\
  \u30C3\u30BB\u30FC\u30B8\u3084\u8A3A\u65AD\u51FA\u529B\u3092\u3001\u6A19\u6E96\u51FA\
  \u529B\uFF08stdout\uFF09\u3068\u306F\u5225\u306E\u30B9\u30C8\u30EA\u30FC\u30E0\u306B\
  \u5411\u3051\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\u308C\
  \u306F\u3001\u6A19\u6E96\u51FA\u529B\u3092\u6DF7\u4E71\u3055\u305B\u308B\u3053\u3068\
  \u306A\u304F\u3001\u30C7\u30D0\u30C3\u30B0\u3084\u30A8\u30E9\u30FC\u30ED\u30B0\u3092\
  \u884C\u3046\u4E0A\u3067\u91CD\u8981\u3067\u3042\u308A\u3001\u958B\u767A\u8005\u3068\
  \u30E6\u30FC\u30B6\u30FC\u53CC\u65B9\u304C\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u72B6\
  \u614B\u3084\u554F\u984C\u3092\u7406\u89E3\u3059\u308B\u306E\u3092\u5BB9\u6613\u306B\
  \u3057\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:01.750482
model: gpt-4-0125-preview
summary: "\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\u306E\u66F8\u304D\
  \u8FBC\u307F\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u30A8\u30E9\u30FC\u30E1\
  \u30C3\u30BB\u30FC\u30B8\u3084\u8A3A\u65AD\u51FA\u529B\u3092\u3001\u6A19\u6E96\u51FA\
  \u529B\uFF08stdout\uFF09\u3068\u306F\u5225\u306E\u30B9\u30C8\u30EA\u30FC\u30E0\u306B\
  \u5411\u3051\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\u308C\
  \u306F\u3001\u6A19\u6E96\u51FA\u529B\u3092\u6DF7\u4E71\u3055\u305B\u308B\u3053\u3068\
  \u306A\u304F\u3001\u30C7\u30D0\u30C3\u30B0\u3084\u30A8\u30E9\u30FC\u30ED\u30B0\u3092\
  \u884C\u3046\u4E0A\u3067\u91CD\u8981\u3067\u3042\u308A\u3001\u958B\u767A\u8005\u3068\
  \u30E6\u30FC\u30B6\u30FC\u53CC\u65B9\u304C\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u72B6\
  \u614B\u3084\u554F\u984C\u3092\u7406\u89E3\u3059\u308B\u306E\u3092\u5BB9\u6613\u306B\
  \u3057\u307E\u3059\u3002"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
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
