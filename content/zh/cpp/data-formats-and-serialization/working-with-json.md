---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:57.287880-07:00
description: "JSON\uFF08JavaScript \u5BF9\u8C61\u8868\u793A\u6CD5\uFF09\u662F\u4E00\
  \u79CD\u8F7B\u91CF\u7EA7\u7684\u6570\u636E\u5B58\u50A8\u4E0E\u4F20\u8F93\u683C\u5F0F\
  \uFF0C\u4F7F\u5176\u6210\u4E3A\u670D\u52A1\u5668\u4E0E\u7F51\u7EDC\u5E94\u7528\u7A0B\
  \u5E8F\u4E4B\u95F4\u6570\u636E\u4EA4\u6362\u7684\u4F18\u79C0\u5A92\u4ECB\u3002\u7A0B\
  \u5E8F\u5458\u4E4B\u6240\u4EE5\u4F7F\u7528 JSON\uFF0C\u662F\u56E0\u4E3A\u5B83\u6613\
  \u4E8E\u4EBA\u7C7B\u9605\u8BFB\u548C\u673A\u5668\u89E3\u6790\uFF0C\u7279\u522B\u662F\
  \u5728\u9700\u8981\u901A\u8FC7\u4E92\u8054\u7F51\u8FDB\u884C\u6570\u636E\u4EA4\u6362\
  \u6216\u914D\u7F6E\u8BBE\u7F6E\u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u3002"
lastmod: '2024-03-13T22:44:48.134743-06:00'
model: gpt-4-0125-preview
summary: "JSON\uFF08JavaScript \u5BF9\u8C61\u8868\u793A\u6CD5\uFF09\u662F\u4E00\u79CD\
  \u8F7B\u91CF\u7EA7\u7684\u6570\u636E\u5B58\u50A8\u4E0E\u4F20\u8F93\u683C\u5F0F\uFF0C\
  \u4F7F\u5176\u6210\u4E3A\u670D\u52A1\u5668\u4E0E\u7F51\u7EDC\u5E94\u7528\u7A0B\u5E8F\
  \u4E4B\u95F4\u6570\u636E\u4EA4\u6362\u7684\u4F18\u79C0\u5A92\u4ECB\u3002\u7A0B\u5E8F\
  \u5458\u4E4B\u6240\u4EE5\u4F7F\u7528 JSON\uFF0C\u662F\u56E0\u4E3A\u5B83\u6613\u4E8E\
  \u4EBA\u7C7B\u9605\u8BFB\u548C\u673A\u5668\u89E3\u6790\uFF0C\u7279\u522B\u662F\u5728\
  \u9700\u8981\u901A\u8FC7\u4E92\u8054\u7F51\u8FDB\u884C\u6570\u636E\u4EA4\u6362\u6216\
  \u914D\u7F6E\u8BBE\u7F6E\u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
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
