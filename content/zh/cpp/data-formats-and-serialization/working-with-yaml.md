---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:45.217286-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728C++\u4E2D\u64CD\u4F5CYAML\uFF0C\
  \u4E00\u4E2A\u6D41\u884C\u7684\u9009\u62E9\u662F\u4F7F\u7528`yaml-cpp`\u5E93\u3002\
  \u9996\u5148\uFF0C\u786E\u4FDD\u4F60\u5DF2\u7ECF\u5B89\u88C5\u5E76\u6B63\u786E\u94FE\
  \u63A5\u4E86`yaml-cpp`\u5230\u4F60\u7684C++\u9879\u76EE\u4E2D\u3002 **\u8BFB\u53D6\
  YAML\u6587\u4EF6\uFF1A**."
lastmod: '2024-04-05T22:38:47.286101-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728C++\u4E2D\u64CD\u4F5CYAML\uFF0C\u4E00\
  \u4E2A\u6D41\u884C\u7684\u9009\u62E9\u662F\u4F7F\u7528`yaml-cpp`\u5E93\u3002\u9996\
  \u5148\uFF0C\u786E\u4FDD\u4F60\u5DF2\u7ECF\u5B89\u88C5\u5E76\u6B63\u786E\u94FE\u63A5\
  \u4E86`yaml-cpp`\u5230\u4F60\u7684C++\u9879\u76EE\u4E2D\u3002 **\u8BFB\u53D6YAML\u6587\
  \u4EF6\uFF1A**."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

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
