---
title:                "使用json进行编程"
html_title:           "Python: 使用json进行编程"
simple_title:         "使用json进行编程"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/working-with-json.md"
---

{{< edit_this_page >}}

## 为什么

如果你想要处理和传输结构化数据，那么JSON是一个非常好的选择。它易于阅读和编写，适用于多种编程语言，而且在网络交互中也非常方便。

## 如何使用

```Python
# 导入json库
import json

# 创建一个字典
person = {
    "name": "张三",
    "age": 25,
    "hobbies": ["篮球", "游泳", "阅读"]
}

# 将字典转换为JSON字符串
json_string = json.dumps(person)

# 打印输出
print(json_string)

# 将JSON字符串转换为字典
new_person = json.loads(json_string)

# 输出字典的值
print(new_person["name"]) # 输出："张三"
print(new_person["age"]) # 输出：25
print(new_person["hobbies"]) # 输出：["篮球", "游泳", "阅读"]
```

## 深入探讨

JSON（JavaScript Object Notation）是一种轻量级的数据交换格式，同时也是一种数据结构。它由键值对组成，支持多种数据类型，包括字符串、数字、数组和对象。在Python中，我们可以通过使用json库来轻松地处理JSON数据。除了上面的示例中使用的dumps()和loads()函数外，我们还可以使用dump()和load()函数来读写JSON文件。此外，使用indent参数可以让我们更方便地阅读格式化过的JSON数据。

## 更多文章

[Python官方文档](https://docs.python.org/3/library/json.html)

[JSON官方网站](https://www.json.org/json-zh.html)

[Python数据交换格式指南](https://docs.python-guide.org/scenarios/json/)

[网站数据采集与解析：JSON篇（Python）](https://zhuanlan.zhihu.com/p/352533426)

```Markdown
## 查看更多

[Python官方文档](https://docs.python.org/3/library/json.html)

[JSON官方网站](https://www.json.org/json-zh.html)

[Python数据交换格式指南](https://docs.python-guide.org/scenarios/json/)

[网站数据采集与解析：JSON篇（Python）](https://zhuanlan.zhihu.com/p/352533426)
```