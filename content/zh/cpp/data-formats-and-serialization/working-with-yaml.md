---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:45.217286-07:00
description: "YAML\uFF0C\u5168\u79F0\u4E3AYAML Ain't Markup Language\uFF08YAML\u4E0D\
  \u662F\u6807\u8BB0\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u4EBA\u7C7B\u53EF\u8BFB\
  \u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\
  \u5B83\u8FDB\u884C\u914D\u7F6E\u6587\u4EF6\u7F16\u5199\u3001\u6570\u636E\u8F6C\u50A8\
  \u548C\u5B58\u50A8\u5C42\u6B21\u5316\u6570\u636E\uFF0C\u56E0\u4E3A\u4E0EXML\u6216\
  JSON\u76F8\u6BD4\uFF0C\u5B83\u7684\u53EF\u8BFB\u6027\u66F4\u9AD8\uFF0C\u8BED\u6CD5\
  \u66F4\u6613\u4E8E\u7406\u89E3\u3002"
lastmod: 2024-02-19 22:05:07.191174
model: gpt-4-0125-preview
summary: "YAML\uFF0C\u5168\u79F0\u4E3AYAML Ain't Markup Language\uFF08YAML\u4E0D\u662F\
  \u6807\u8BB0\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u4EBA\u7C7B\u53EF\u8BFB\u7684\
  \u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\
  \u8FDB\u884C\u914D\u7F6E\u6587\u4EF6\u7F16\u5199\u3001\u6570\u636E\u8F6C\u50A8\u548C\
  \u5B58\u50A8\u5C42\u6B21\u5316\u6570\u636E\uFF0C\u56E0\u4E3A\u4E0EXML\u6216JSON\u76F8\
  \u6BD4\uFF0C\u5B83\u7684\u53EF\u8BFB\u6027\u66F4\u9AD8\uFF0C\u8BED\u6CD5\u66F4\u6613\
  \u4E8E\u7406\u89E3\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
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
