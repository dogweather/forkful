---
date: 2024-01-26 04:26:47.974199-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u9996\u5148\uFF0C\u60A8\u9700\u8981\u4E00\
  \u4E2ATOML\u89E3\u6790\u5668\u3002Swift\u6CA1\u6709\u5185\u7F6E\u7684\u89E3\u6790\
  \u5668\uFF0C\u6240\u4EE5\u8BA9\u6211\u4EEC\u4F7F\u7528`TOMLDecoder`\u3002\u901A\u8FC7\
  Swift\u5305\u7BA1\u7406\u5668\u5B89\u88C5\u5B83\uFF0C\u7136\u540E\u53EF\u4EE5\u8F7B\
  \u677E\u5730\u5E8F\u5217\u5316\u548C\u53CD\u5E8F\u5217\u5316TOML\u3002"
lastmod: '2024-04-05T21:53:48.472863-06:00'
model: gpt-4-0125-preview
summary: "\u9996\u5148\uFF0C\u60A8\u9700\u8981\u4E00\u4E2ATOML\u89E3\u6790\u5668\u3002\
  Swift\u6CA1\u6709\u5185\u7F6E\u7684\u89E3\u6790\u5668\uFF0C\u6240\u4EE5\u8BA9\u6211\
  \u4EEC\u4F7F\u7528`TOMLDecoder`\u3002\u901A\u8FC7Swift\u5305\u7BA1\u7406\u5668\u5B89\
  \u88C5\u5B83\uFF0C\u7136\u540E\u53EF\u4EE5\u8F7B\u677E\u5730\u5E8F\u5217\u5316\u548C\
  \u53CD\u5E8F\u5217\u5316TOML\u3002"
title: "\u4F7F\u7528TOML"
weight: 39
---

## 如何操作:
首先，您需要一个TOML解析器。Swift没有内置的解析器，所以让我们使用`TOMLDecoder`。通过Swift包管理器安装它，然后可以轻松地序列化和反序列化TOML。

```Swift
import TOMLDecoder

let tomlString = """
title = "TOML示例"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("标题: \(config.title), 拥有者: \(config.owner.name), 出生日期: \(config.owner.dob)")
    } catch {
        print("解析TOML时出错: \(error)")
    }
}
```

这段代码输出：
```
标题: TOML示例, 拥有者: Tom Preston-Werner, 出生日期: 1979-05-27 07:32:00 +0000
```

## 深入了解
TOML由GitHub的联合创始人Tom Preston-Werner设计，作为对格式如JSON或YAML的更加人性化的替代方案。它的目标是清晰性，减少人类或机器误解的可能。就替代方案而言，YAML和JSON是常见的选择，YAML倾向于人类的可读性，而JSON则作为更简单的机器友好选项。在Swift中处理TOML时，我们没有一个原生的解析器。然而，第三方库如`TOMLDecoder`通过Swift 4中引入的`Codable`协议，便于TOML字符串和Swift类型之间的轻松转换，从而简化了序列化过程。

## 另请参阅
- TOML标准：https://toml.io
- `TOMLDecoder`的GitHub：https://github.com/dduan/TOMLDecoder
- Swift关于`Codable`的文档：https://developer.apple.com/documentation/swift/codable
- 数据序列化格式的比较：https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
