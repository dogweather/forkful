---
date: 2024-01-26 04:19:53.393650-07:00
description: "TOML\uFF08Tom's Obvious, Minimal Language\uFF09\uFF0C\u5373Tom\u7684\
  \u660E\u4E86\u3001\u6700\u5C0F\u8BED\u8A00\uFF0C\u662F\u4E00\u79CD\u6613\u4E8E\u9605\
  \u8BFB\u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\uFF0C\u56E0\u5176\u660E\u786E\
  \u7684\u8BED\u4E49\u800C\u6613\u4E8E\u9605\u8BFB\u3002\u7A0B\u5E8F\u5458\u7528TOML\u6765\
  \u7F16\u5199\u914D\u7F6E\u6587\u4EF6\uFF0C\u56E0\u4E3A\u5B83\u5728\u4EBA\u7C7B\u53EF\
  \u8BFB\u6027\u548C\u673A\u5668\u53EF\u89E3\u6790\u6027\u4E4B\u95F4\u53D6\u5F97\u4E86\
  \u5E73\u8861\u3002"
lastmod: '2024-03-13T22:44:48.136705-06:00'
model: gpt-4-0125-preview
summary: "TOML\uFF08Tom's Obvious, Minimal Language\uFF09\uFF0C\u5373Tom\u7684\u660E\
  \u4E86\u3001\u6700\u5C0F\u8BED\u8A00\uFF0C\u662F\u4E00\u79CD\u6613\u4E8E\u9605\u8BFB\
  \u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\uFF0C\u56E0\u5176\u660E\u786E\u7684\
  \u8BED\u4E49\u800C\u6613\u4E8E\u9605\u8BFB\u3002\u7A0B\u5E8F\u5458\u7528TOML\u6765\
  \u7F16\u5199\u914D\u7F6E\u6587\u4EF6\uFF0C\u56E0\u4E3A\u5B83\u5728\u4EBA\u7C7B\u53EF\
  \u8BFB\u6027\u548C\u673A\u5668\u53EF\u89E3\u6790\u6027\u4E4B\u95F4\u53D6\u5F97\u4E86\
  \u5E73\u8861\u3002."
title: "\u4F7F\u7528TOML"
weight: 39
---

## 什么和为什么？
TOML（Tom's Obvious, Minimal Language），即Tom的明了、最小语言，是一种易于阅读的数据序列化格式，因其明确的语义而易于阅读。程序员用TOML来编写配置文件，因为它在人类可读性和机器可解析性之间取得了平衡。

## 如何操作：
要在C++中使用TOML，你需要一个像`toml++`这样的库。这里是一个快速开始：

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // 从文件解析TOML
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // 访问一个值
    std::string title = config["title"].value_or("Untitled");
    std::cout << "标题：" << title << '\n';

    // 修改并保存TOML
    config["title"] = "新标题";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

示例`config.toml`：
```toml
title = "示例"
```

示例输出：
```plaintext
标题：示例
```

## 深入了解
TOML由Tom Preston-Werner于2013年创建，作为YAML和JSON的一种替代品。它旨在简单和明确，主要用于配置文件。与JSON不同，TOML注重消除歧义，这意味着文档的解析方式是确定的。

TOML的替代品包括YAML，它在允许的内容方面更加宽松，尽管有时这会以预测性为代价。另一种选择是JSON，它在结构上非常严格，但由于缺乏注释和括号繁多的语法，JSON对配置不如人类友好。

在实现上，`toml++`是一个仅头文件的C++17库，符合最新的TOML规范。它提供了一个DOM类接口，用于导航和操作TOML数据，使其易于集成到项目中。该库负责解析、验证和输出生成，允许你使用C++类型获取和设置TOML数据。

## 另请参阅
- TOML GitHub仓库：https://github.com/toml-lang/toml
- `toml++`，一个C++的TOML库：https://github.com/marzer/tomlplusplus
- 官方TOML文档，提供了格式的详细解释：https://toml.io/en/
