---
title:                "使用YAML工作"
aliases:
- /zh/cpp/working-with-yaml.md
date:                  2024-02-03T19:24:45.217286-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

YAML，全称为YAML Ain't Markup Language（YAML不是标记语言），是一种人类可读的数据序列化格式。程序员使用它进行配置文件编写、数据转储和存储层次化数据，因为与XML或JSON相比，它的可读性更高，语法更易于理解。

## 如何操作：

在C++中操作YAML，一个流行的选择是使用`yaml-cpp`库。首先，确保你已经安装并正确链接了`yaml-cpp`到你的C++项目中。

**读取YAML文件：**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "标题: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

给定一个如下所示的`config.yaml`：

```yaml
title: "示例YAML"
```

运行上述C++代码将产生：

```
标题: 示例YAML
```

**写入YAML文件：**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "title" << YAML::Value << "示例YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

这段代码将创建一个内容如下的`output.yaml`：

```yaml
title: 示例YAML
```

这些例子作为使用`yaml-cpp`库在C++中读写YAML文件的基础介绍。对于更复杂的结构和用例，请探索`yaml-cpp`文档，了解序列、标签以及更高级的序列化和反序列化技术等特性。
