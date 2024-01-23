---
title:                "处理JSON数据"
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? 什么及为什么?
JSON (JavaScript Object Notation) 是数据交换的轻量级格式。C++程序员使用它来与Web服务交互和配置文件处理，因为它易读且易于机器解析。

## How to: 如何操作
首先，你需要选择一个C++ JSON库，例如 [nlohmann/json](https://github.com/nlohmann/json)。下面是一个基本的代码示例：

```C++
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // JSON对象创建
    nlohmann::json obj;
    obj["name"] = "张三";
    obj["age"] = 30;
    obj["is_programmer"] = true;

    // 转换为字符串并输出
    std::string json_string = obj.dump();
    std::cout << json_string << std::endl;

    // 解析JSON字符串
    auto parsed = nlohmann::json::parse(json_string);
    std::cout << parsed["name"] << std::endl; // 输出: 张三
}
```

输出:
```
{"age":30,"is_programmer":true,"name":"张三"}
张三
```

## Deep Dive 深入探讨
JSON诞生于2001年，由Douglas Crockford推广。它作为XML的一个简单替代品逐渐受到欢迎。除了nlohmann/json，其他C++库比如RapidJSON和JsonCpp也提供JSON支持。从C++11开始，语言的特性如智能指针和`std::nullptr`使得处理JSON更加直观。

## See Also 另请参阅
- 官方JSON网站: [json.org](https://www.json.org/json-zh.html)
- nlohmann/json GitHub仓库: [nlohmann/json](https://github.com/nlohmann/json)
- JSON for Modern C++ 用户指南: [Read the Docs](https://nlohmann.github.io/json/)
- 如何在C++中使用RapidJSON：[RapidJSON Documentation](http://rapidjson.org/md_doc_tutorial.html)
- JsonCpp GitHub仓库：[JsonCpp](https://github.com/open-source-parsers/jsoncpp)
