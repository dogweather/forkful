---
title:                "使用JSON编程"
html_title:           "Fish Shell: 使用JSON编程"
simple_title:         "使用JSON编程"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

JSON是一种常见的数据格式，它是一种轻量级的、易读写的数据交换格式。程序员们经常使用JSON来存储和传输数据，因为它可以方便地在不同的系统和语言之间进行交流。

## 如何：

在Fish Shell中，可以使用内置的json工具来方便地处理JSON数据。例如，我们可以使用json parse命令来解析JSON数据，并使用.操作符来获取数据中的特定值。下面是一个使用json parse命令的例子：

```
set json_data (curl -s "https://api.github.com/users/fish-shell")
echo $json_data | json parse .login
```

上面的命令将从GitHub的API接口获取Fish Shell用户的信息，并输出用户名"fish-shell"。

## 深入探讨：

JSON最早是由Douglas Crockford在2001年提出的，它是一种基于JavaScript的数据表示方式。由于其简洁易懂的特点，JSON很快就受到了广泛的欢迎，并被广泛应用于Web编程和API接口的数据交换中。

除了使用内置的json工具来处理JSON数据外，也可以使用第三方库，如jq来处理复杂的JSON数据结构。另外，Fish Shell也提供了一个json_array函数，可以方便地将数组转换为JSON格式。

## 参考资料：

- [JSON官方网站](https://www.json.org/)
- [Fish Shell文档中关于JSON的介绍](https://fishshell.com/docs/current/tutorial.html#working-with-json-data)
- [jq官方文档](https://stedolan.github.io/jq/)