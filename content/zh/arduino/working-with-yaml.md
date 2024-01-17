---
title:                "使用yaml的计算机编程"
html_title:           "Arduino: 使用yaml的计算机编程"
simple_title:         "使用yaml的计算机编程"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

# 关于Arduino中使用YAML的简介

YAML是一种用来存储数据的格式，它可以被程序员用来读取和写入数据。程序员使用YAML来存储和共享配置文件、数据库设置和其他任何需要存储的结构化数据。

## 如何使用YAML

使用YAML的第一步是导入相关的库。Arduino IDE已经预装了YAML库，您只需要选择“库管理器”并在搜索栏中输入“YAML”就可以找到它。导入YAML库后，您就可以开始编写代码来读取和写入YAML文件了。

### 如何读取YAML文件：

```
Arduino ...
#include <YAML.h>
...
YAML.load("config.yaml");
```

### 如何写入YAML文件：

```
Arduino ...
#include <YAML.h>
...
YAML.begin();
YAML.addValue("name", "Arduino");
YAML.addValue("version", "1.8.12");
YAML.end("config.yaml");
```

## 深入了解

YAML是一种轻量级的数据格式，在2001年被开发出来。与JSON和XML相比，YAML更易读，因为它使用缩进来表示层级关系。与XML相比，YAML也更容易编辑和修改，因为它不需要闭合标签。在Arduino中，除了YAML，您也可以使用JSON或XML来存储数据。

## 参考资料

如果您想了解更多关于YAML的内容，可以参考以下链接：

1. YAML官方网站：https://yaml.org/
2. YAML库文档：https://arduinojson.org/doc/api/classes/yaml-document/