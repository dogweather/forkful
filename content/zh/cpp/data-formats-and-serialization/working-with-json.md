---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:57.287880-07:00
description: "\u5982\u4F55\u8FDB\u884C\uFF1A \u5728 C++\u4E2D\uFF0C\u6CA1\u6709\u5BF9\
  \ JSON \u7684\u539F\u751F\u652F\u6301\uFF0C\u4F46\u7B2C\u4E09\u65B9\u5E93\u5982\
  \ nlohmann/json \u7684\u4F7F\u7528\u4F7F\u4E4B\u53D8\u5F97\u7B80\u5355\u3002\u4EE5\
  \u4E0B\u662F\u5176\u57FA\u672C\u4EFB\u52A1\u7684\u4F7F\u7528\u65B9\u6CD5\uFF1A \u9996\
  \u5148\uFF0C\u786E\u4FDD\u4F60\u5B89\u88C5\u4E86\u5E93\u3002\u5982\u679C\u4F60\u4F7F\
  \u7528\u7684\u662F\u50CF vcpkg \u6216 Conan \u8FD9\u6837\u7684\u5305\u7BA1\u7406\
  \u5668\uFF0C\u4F60\u53EF\u4EE5\u8F7B\u677E\u5730\u5C06 `nlohmann/json` \u6DFB\u52A0\
  \u5230\u4F60\u7684\u9879\u76EE\u4E2D\u3002"
lastmod: '2024-04-05T22:38:47.287323-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u8FDB\u884C\uFF1A \u5728 C++\u4E2D\uFF0C\u6CA1\u6709\u5BF9\
  \ JSON \u7684\u539F\u751F\u652F\u6301\uFF0C\u4F46\u7B2C\u4E09\u65B9\u5E93\u5982\
  \ nlohmann/json \u7684\u4F7F\u7528\u4F7F\u4E4B\u53D8\u5F97\u7B80\u5355\u3002\u4EE5\
  \u4E0B\u662F\u5176\u57FA\u672C\u4EFB\u52A1\u7684\u4F7F\u7528\u65B9\u6CD5\uFF1A \u9996\
  \u5148\uFF0C\u786E\u4FDD\u4F60\u5B89\u88C5\u4E86\u5E93\u3002\u5982\u679C\u4F60\u4F7F\
  \u7528\u7684\u662F\u50CF vcpkg \u6216 Conan \u8FD9\u6837\u7684\u5305\u7BA1\u7406\
  \u5668\uFF0C\u4F60\u53EF\u4EE5\u8F7B\u677E\u5730\u5C06 `nlohmann/json` \u6DFB\u52A0\
  \u5230\u4F60\u7684\u9879\u76EE\u4E2D\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

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
