---
title:                "使用JSON进行编程"
aliases:
- zh/cpp/working-with-json.md
date:                  2024-02-03T19:21:57.287880-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

JSON（JavaScript 对象表示法）是一种轻量级的数据存储与传输格式，使其成为服务器与网络应用程序之间数据交换的优秀媒介。程序员之所以使用 JSON，是因为它易于人类阅读和机器解析，特别是在需要通过互联网进行数据交换或配置设置的应用程序中。

## 如何进行：

在 C++中，没有对 JSON 的原生支持，但第三方库如 nlohmann/json 的使用使之变得简单。以下是其基本任务的使用方法：

首先，确保你安装了库。如果你使用的是像 vcpkg 或 Conan 这样的包管理器，你可以轻松地将 `nlohmann/json` 添加到你的项目中。

### 从字符串解析 JSON

```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // JSON 数据字符串
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // 解析 JSON 字符串
    auto jsonObject = nlohmann::json::parse(jsonData);

    // 访问数据
    std::cout << "姓名: " << jsonObject["name"] << "\n"
              << "年龄: " << jsonObject["age"] << "\n"
              << "城市: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**示例输出：**

```
姓名: John
年龄: 30
城市: New York
```

### 生成 JSON

生成 JSON 数据同样直接；你只需将值赋给 `nlohmann::json` 对象。

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // 创建一个 JSON 对象
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // 将 JSON 对象转换为字符串并打印
    std::string jsonString = jsonObject.dump(4); // 参数 4 用于美化打印
    std::cout << jsonString << std::endl;

    return 0;
}
```

**示例输出：**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

这些示例演示了使用 `nlohmann/json` 库在 C++ 中处理 JSON 的核心功能。有了这些基础，你可以解析和生成 JSON 以用于各种应用，从配置文件到网络应用程序中的数据交换。
