---
title:                "与Yaml一起工作"
html_title:           "C++: 与Yaml一起工作"
simple_title:         "与Yaml一起工作"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

# YAML与C++编程

## 什么是 YAML？

YAML是一种轻量级的格式化语言，它旨在以易读易写的方式来描述数据。它可以被用来作为配置文件或交换数据的格式，特别适合于结构化数据的表示。

## 为什么程序员使用它？

程序员通常使用YAML格式的文件来存储和读取配置信息。相比于其他格式，YAML更加人性化和易于阅读，即使是非程序员也可以轻松地理解它。另外，YAML也被广泛用于Web应用程序的数据交换，因为它支持结构化数据的表示。

## 如何使用 YAML？

### 示例1：使用YAML读取配置文件

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>

int main() {
  // 读取配置文件
  YAML::Node config = YAML::LoadFile("config.yaml");

  // 打印配置信息
  std::cout << "用户名：" << config["username"].as<std::string>() << std::endl;
  std::cout << "密码：" << config["password"].as<std::string>() << std::endl;
  std::cout << "端口号：" << config["port"].as<int>() << std::endl;
  std::cout << "是否启用HTTPS：" << std::boolalpha << config["use_https"].as<bool>() << std::endl;

  return 0;
}
```

**输出：**
```
用户名：John Doe
密码：12345
端口号：8080
是否启用HTTPS：true
```

### 示例2：使用YAML写入配置文件

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>

int main() {
  // 创建YAML节点
  YAML::Node config;

  // 设置配置信息
  config["username"] = "John Doe";
  config["password"] = "12345";
  config["port"] = 8080;
  config["use_https"] = true;

  // 写入配置文件
  std::ofstream fout("config.yaml");
  fout << config;

  return 0;
}
```

**输出：**
```
username: John Doe
password: "12345"
port: 8080
use_https: true
```

## 深入了解

### 历史背景

YAML最初于2001年由Clark Evans基于YAML Ain't Markup Language的思想开发而成。它的设计目的是为了提供一种易读易写的格式化语言来表示数据。目前，YAML已被广泛应用于多种编程语言和各种类型的应用程序中。

### 替代选择

与YAML类似的格式化语言还有XML和JSON。它们都有各自的优缺点，程序员可以根据实际需求来选择最适合自己的格式。

### 实现细节

YAML的C++实现是通过yaml-cpp库来实现的。该库提供了一套用于读取和写入YAML文件的API，可以方便地在C++中使用。该库由Ryan Pavlik维护，并在github上开源。

## 相关资源

- [YAML官方网站](https://yaml.org/)
- [yaml-cpp github仓库](https://github.com/jbeder/yaml-cpp)
- [YAML教程](https://learnxinyminutes.com/docs/yaml/)
- [与YAML相关的C++实践指南](https://www.codeproject.com/articles/816985/yaml-cpp-how-to-read-a-yaml-file-using-yaml-cpp)