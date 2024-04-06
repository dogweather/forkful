---
date: 2024-01-26 04:19:53.393650-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728C++\u4E2D\u4F7F\u7528TOML\uFF0C\
  \u4F60\u9700\u8981\u4E00\u4E2A\u50CF`toml++`\u8FD9\u6837\u7684\u5E93\u3002\u8FD9\
  \u91CC\u662F\u4E00\u4E2A\u5FEB\u901F\u5F00\u59CB\uFF1A."
lastmod: '2024-04-05T21:53:48.428757-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528TOML"
weight: 39
---

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
