---
title:                "处理 YAML 文件"
date:                  2024-01-19
simple_title:         "处理 YAML 文件"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么?)
YAML 是一种简洁的数据序列化格式，适合配置文件和数据交换。程序员使用 YAML 因为它比 JSON 和 XML 更易读，且能轻松映射到程序语言的数据结构。

## How to: (如何操作)
在 C++ 中处理 YAML 首先需要安装一个库，比如 `yaml-cpp`。安装后，你可以这样解析 YAML:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream fin("config.yaml");
    YAML::Node config = YAML::Load(fin);
    
    std::string username = config["username"].as<std::string>();
    std::cout << "Username: " << username << std::endl;
}
```

假设 `config.yaml` 内容如下：

```yaml
username: ZhangSan
```

则输出将会是：

```
Username: ZhangSan
```

## Deep Dive (深入探讨)
YAML 在 2001 年出现，目标是易于人类阅读和机器解析。JSON 和 XML 是两种流行的替代格式; JSON 更简洁，XML 更严格。 `yaml-cpp` 是一个流行的 C++ YAML 解析器，提供了序列化和反序列化的功能，但需手动管理内存和处理异常。

## See Also (另见)
- 官方网站 [YAML](https://yaml.org)
- `yaml-cpp` GitHub [页面](https://github.com/jbeder/yaml-cpp)
- JSON [介绍](https://www.json.org/json-zh.html)
- XML [教程](https://www.w3schools.com/xml/xml_whatis.asp)
