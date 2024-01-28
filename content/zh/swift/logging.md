---
title:                "日志记录"
date:                  2024-01-26T01:09:08.881223-07:00
model:                 gpt-4-1106-preview
simple_title:         "日志记录"
programming_language: "Swift"
category:             "Swift"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/logging.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
日志记录是指将应用程序的行为、错误以及其他重要信息记录在持久化介质上，比如文件或数据库。程序员这么做是为了跟踪应用的健康和性能，调试问题，以及在生产环境下密切关注底层正在发生的事情。

## 如何操作：
在Swift中，你可以使用print语句将日志写入控制台，或者使用更灵活的`os.log` API，它挂钩到苹果平台上的统一日志系统。

```Swift
import os.log

let logger = OSLog(subsystem: "com.yourapp.domain", category: "network")

func fetchData() {
    // 简单的print语句
    print("开始获取数据")
    
    // 使用os.log记录信息级别事件
    os_log(.info, log: logger, "从API获取数据。")
    
    do {
        let data = try performNetworkRequest()
        // 记录调试级别事件
        os_log(.debug, log: logger, "接收到数据：%@", data.description)
    } catch {
        // 记录错误级别事件
        os_log(.error, log: logger, "获取数据失败：%@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // 模拟网络请求
    return Data()
}
```

控制台上的示例输出可能如下所示：

```
开始获取数据
正在从API获取数据。
接收到数据：一些数据字节...
```

对于错误，可能是：

```
获取数据失败：互联网连接似乎离线了。
```

## 深入了解
在iOS 10和macOS Sierra引入的统一日志系统中，Swift的日志记录具有了新的能力和效率。与直接输出到控制台的`print`语句不同，这个系统是基于活动的，并允许你根据日志消息的重要性以及它们是调试还是发布构建来进行过滤。

历史背景勾勒出了在iOS和macOS中日志记录从基础print语句向集成了Instruments应用和控制台的综合工具的演变，提供了复杂的日志分析方式。

Swift内部有多种日志替代方案，如第三方库CocoaLumberjack，它在统一日志系统之上提供了宏层。它提供了对日志格式、文件管理和性能选项的增强控制。

最后是实施细节；OSLog不仅设计得高效，而且重视隐私，能够在记录日志时对私人数据进行模糊处理。它将日志分为错误、信息和调试等级别，每个级别都提供了不同粒度的故障排查。

## 另请参见
- [苹果的统一日志文档](https://developer.apple.com/documentation/os/logging)
- [Ray Wenderlich的日志记录教程](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [CocoaLumberjack GitHub仓库](https://github.com/CocoaLumberjack/CocoaLumberjack)
