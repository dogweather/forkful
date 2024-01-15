---
title:                "使用 yaml 进行编程"
html_title:           "Fish Shell: 使用 yaml 进行编程"
simple_title:         "使用 yaml 进行编程"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## 为什么
首先，YAML是一种易于阅读和编写的格式，适用于存储和传输数据。其次，它已成为许多编程语言和工具中常用的配置文件格式。

## 如何
```Fish Shell（Fish Shell是一种流行的命令行Shell，类似于Bash）语法可以轻松解析和使用YAML格式的数据，例如：```

```shell
set data (yaml load "config.yaml")
echo $data
```

注意，需要在使用之前安装Fish Shell的yaml插件。 

## 深入了解
YAML具有类似于JSON的键值对结构，但其语法更加简洁和易读。它也支持多种数据类型，例如字符串、整数、浮点数和布尔值等。此外，YAML还可以嵌套数据结构，使其在复杂的配置文件中能够更好地组织数据。

## 参考资料
- [Fish Shell官方网站](https://fishshell.com/)
- [YAML官方网站](https://yaml.org/)
- [Fish Shell的yaml插件](https://fishshell.com/docs/current/commands.html#yaml)