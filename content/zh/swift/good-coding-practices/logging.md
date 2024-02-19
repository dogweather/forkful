---
aliases:
- /zh/swift/logging/
date: 2024-01-26 01:09:08.881223-07:00
description: "\u65E5\u5FD7\u8BB0\u5F55\u662F\u6307\u5C06\u5E94\u7528\u7A0B\u5E8F\u7684\
  \u884C\u4E3A\u3001\u9519\u8BEF\u4EE5\u53CA\u5176\u4ED6\u91CD\u8981\u4FE1\u606F\u8BB0\
  \u5F55\u5728\u6301\u4E45\u5316\u4ECB\u8D28\u4E0A\uFF0C\u6BD4\u5982\u6587\u4EF6\u6216\
  \u6570\u636E\u5E93\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u8DDF\
  \u8E2A\u5E94\u7528\u7684\u5065\u5EB7\u548C\u6027\u80FD\uFF0C\u8C03\u8BD5\u95EE\u9898\
  \uFF0C\u4EE5\u53CA\u5728\u751F\u4EA7\u73AF\u5883\u4E0B\u5BC6\u5207\u5173\u6CE8\u5E95\
  \u5C42\u6B63\u5728\u53D1\u751F\u7684\u4E8B\u60C5\u3002"
lastmod: 2024-02-18 23:08:59.447773
model: gpt-4-1106-preview
summary: "\u65E5\u5FD7\u8BB0\u5F55\u662F\u6307\u5C06\u5E94\u7528\u7A0B\u5E8F\u7684\
  \u884C\u4E3A\u3001\u9519\u8BEF\u4EE5\u53CA\u5176\u4ED6\u91CD\u8981\u4FE1\u606F\u8BB0\
  \u5F55\u5728\u6301\u4E45\u5316\u4ECB\u8D28\u4E0A\uFF0C\u6BD4\u5982\u6587\u4EF6\u6216\
  \u6570\u636E\u5E93\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u8DDF\
  \u8E2A\u5E94\u7528\u7684\u5065\u5EB7\u548C\u6027\u80FD\uFF0C\u8C03\u8BD5\u95EE\u9898\
  \uFF0C\u4EE5\u53CA\u5728\u751F\u4EA7\u73AF\u5883\u4E0B\u5BC6\u5207\u5173\u6CE8\u5E95\
  \u5C42\u6B63\u5728\u53D1\u751F\u7684\u4E8B\u60C5\u3002"
title: "\u65E5\u5FD7\u8BB0\u5F55"
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
