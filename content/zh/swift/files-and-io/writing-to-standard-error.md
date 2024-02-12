---
title:                "写入标准错误"
aliases:
- /zh/swift/writing-to-standard-error/
date:                  2024-02-03T19:34:50.088754-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 是什么&为什么？

将错误消息或诊断输出定向到标准错误（stderr）是指将程序的错误消息或诊断输出重定向到一个与标准输出（stdout）不同的流。这对于调试和记录错误而不弄乱标准输出至关重要，有助于开发者和用户理解程序的状态和问题。

## 如何操作：

在Swift中，可以使用`FileHandle`类直接访问stderr来写入标准错误。以下是一个简单示例：

```swift
import Foundation

// 定义一条消息
let errorMessage = "发生了一个错误。\n"

// 将消息转换为数据
if let data = errorMessage.data(using: .utf8) {
    // 将错误消息写入stderr
    FileHandle.standardError.write(data)
}
```

输出到stderr（通常在控制台或终端中查看）：
```
发生了一个错误。
```

对于更复杂的日志记录或在使用外部库时，可能会考虑使用第三方库，如**SwiftLog**。尽管**SwiftLog**没有直接支持写入stderr，但你可以实现一个自定义的日志后端来实现这一点。以下是定义一个将日志写入stderr的自定义日志处理程序的简化示例：

首先，在`Package.swift`中将**SwiftLog**添加到项目依赖项中：
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "你的包名",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "你的目标名称",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
            ]),
    ]
)
```

然后，实现一个将日志写入stderr的自定义日志处理程序：

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

// 使用方式
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.example.yourapp")

logger.error("这是一条错误消息")
```

输出到stderr：
```
这是一条错误消息
```

这个自定义处理程序允许你将SwiftLog的错误消息直接路由到标准错误，与应用程序可能生成的其他日志消息无缝集成。
