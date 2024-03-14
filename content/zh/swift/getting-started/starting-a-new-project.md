---
date: 2024-01-20 18:04:35.834748-07:00
description: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE\u610F\u5473\u7740\u521B\u5EFA\
  \u4E86\u4E00\u4E2A\u7A7A\u767D\u7684\u7F16\u7A0B\u73AF\u5883\uFF0C\u7528\u4E8E\u5F00\
  \u53D1\u5168\u65B0\u7684\u8F6F\u4EF6\u6216\u5E94\u7528\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u662F\u4E3A\u4E86\u5B9E\u73B0\u65B0\u7684\u60F3\u6CD5\uFF0C\u5F00\u53D1\
  \u89E3\u51B3\u65B9\u6848\uFF0C\u6216\u8005\u5B66\u4E60\u65B0\u6280\u80FD\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.158443-06:00'
model: gpt-4-1106-preview
summary: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE\u610F\u5473\u7740\u521B\u5EFA\
  \u4E86\u4E00\u4E2A\u7A7A\u767D\u7684\u7F16\u7A0B\u73AF\u5883\uFF0C\u7528\u4E8E\u5F00\
  \u53D1\u5168\u65B0\u7684\u8F6F\u4EF6\u6216\u5E94\u7528\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u662F\u4E3A\u4E86\u5B9E\u73B0\u65B0\u7684\u60F3\u6CD5\uFF0C\u5F00\u53D1\
  \u89E3\u51B3\u65B9\u6848\uFF0C\u6216\u8005\u5B66\u4E60\u65B0\u6280\u80FD\u3002"
title: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)

开始一个新项目意味着创建了一个空白的编程环境，用于开发全新的软件或应用。程序员这么做是为了实现新的想法，开发解决方案，或者学习新技能。

## How to (如何操作):

在Swift中创建新项目，我们通常使用Xcode，这是苹果提供的集成开发环境。

```Swift
// 打开Xcode
// 选择 File > New > Project
// 选择模板，例如 Single View App
// 输入项目名称，例如 MyNewApp
// 设置其他选项，如团队、组织名称和标识、接口和语言选项
// 选择保存位置后，点击创建
```

创建后你会看到如下的基础代码结构：

```Swift
import UIKit

class ViewController: UIViewController {

    override func viewDidLoad() {
        super.viewDidLoad()
        // 这里是进行初始化的好地方
    }
}
```

运行项目，初始界面会展示在你的模拟器或者设备上。

## Deep Dive (深入探索):

在历史上，Swift项目的创建可能不一定依赖Xcode，尤其是在Swift推出初期的时候，一些开发者会选择使用命令行工具。如今，虽然依然可以使用Swift Package Manager从命令行开始项目，但Xcode提供了更多的便利性，特别是对于图形界面应用程序。

启动新项目时，有若干个选择节点，差异在于应用的类型和目标平台。iOS应用程序和macOS应用程序有着不同的模板和需求。依据项目类型，可选择不同的架构和设计模式，例如MVC (Model-View-Controller) 或MVVM (Model-View-ViewModel)。

除了Xcode以外，也有其他工具可以用来创建Swift项目，例如AppCode，但它们依然远没有Xcode来得流行或集成地深入。

## See Also (另请参阅):

- [Apple's Swift Documentation](https://docs.swift.org/swift-book/)
- [Xcode Help](https://help.apple.com/xcode/mac/current/)
- [Swift Package Manager](https://swift.org/package-manager/)
- [Choosing the right design pattern for your Swift app](https://developer.apple.com/design/human-interface-guidelines/ios/overview/themes/)
