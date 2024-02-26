---
date: 2024-01-26 04:26:47.974199-07:00
description: "TOML\uFF08Tom's Obvious, Minimal Language\uFF0C\u6C64\u59C6\u7684\u660E\
  \u4E86\u3001\u6700\u5C0F\u5316\u8BED\u8A00\uFF09\u662F\u4E00\u79CD\u6570\u636E\u5E8F\
  \u5217\u5316\u683C\u5F0F\uFF0C\u7531\u4E8E\u5176\u6E05\u6670\u7684\u8BED\u4E49\u800C\
  \u6613\u4E8E\u9605\u8BFB\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528TOML\u8FDB\u884C\u914D\
  \u7F6E\u6587\u4EF6\uFF0C\u5176\u4E2D\u4EBA\u7C7B\u7684\u53EF\u8BFB\u6027\u548C\u673A\
  \u5668\u7684\u6613\u89E3\u6790\u6027\u662F\u5173\u952E\u3002"
lastmod: '2024-02-25T18:49:45.752041-07:00'
model: gpt-4-0125-preview
summary: "TOML\uFF08Tom's Obvious, Minimal Language\uFF0C\u6C64\u59C6\u7684\u660E\u4E86\
  \u3001\u6700\u5C0F\u5316\u8BED\u8A00\uFF09\u662F\u4E00\u79CD\u6570\u636E\u5E8F\u5217\
  \u5316\u683C\u5F0F\uFF0C\u7531\u4E8E\u5176\u6E05\u6670\u7684\u8BED\u4E49\u800C\u6613\
  \u4E8E\u9605\u8BFB\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528TOML\u8FDB\u884C\u914D\u7F6E\
  \u6587\u4EF6\uFF0C\u5176\u4E2D\u4EBA\u7C7B\u7684\u53EF\u8BFB\u6027\u548C\u673A\u5668\
  \u7684\u6613\u89E3\u6790\u6027\u662F\u5173\u952E\u3002"
title: "\u4F7F\u7528TOML"
---

{{< edit_this_page >}}

## 什么 & 为什么?
TOML（Tom's Obvious, Minimal Language，汤姆的明了、最小化语言）是一种数据序列化格式，由于其清晰的语义而易于阅读。程序员使用TOML进行配置文件，其中人类的可读性和机器的易解析性是关键。

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
