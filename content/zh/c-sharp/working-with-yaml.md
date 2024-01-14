---
title:                "C#: 使用yaml编程"
simple_title:         "使用yaml编程"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

# 为什么使用YAML

YAML是一种人类友好的数据序列化格式，主要用于配置文件和传输数据。它可以帮助开发人员更有效地存储和传输数据，同时保持易读易用的特性。

## 如何使用

```C#
// 创建一个YAML配置文件
var yaml = new YamlStream();
yaml.Load(new StreamReader(@"config.yaml"));

// 获取配置值
var config = (YamlMappingNode)yaml.Documents[0].RootNode;
var value = config["key"];

// 设置配置值
config["new_key"] = 100;

// 保存配置文件
using (var writer = new StringWriter())
{
    yaml.Save(writer, false);
    Console.WriteLine(writer.ToString());
}

// Output:
// new_key: 100
// key: value
```

## 深入探讨

YAML不仅仅是一个简单的键值对格式，它还支持复杂的数据结构，如列表、嵌套结构等。它也可以引用其他YAML文件，使得配置文件更加灵活。此外，YAML还具有注释、包含变量等特性，使得它成为一个强大的配置工具。

# 参考链接

- [YAML规范官方网站](https://yaml.org/)
- [YamlDotNet文档](https://github.com/aaubry/YamlDotNet/wiki)