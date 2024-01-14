---
title:                "Python: 使用yaml进行编程"
simple_title:         "使用yaml进行编程"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## 为什么

在编程世界中，有许多不同的数据格式，例如JSON、XML和YAML。在这些格式中，YAML是一个简单易懂的格式，也被广泛运用于编程。使用YAML可以帮助程序员更有效地存储和处理数据，因此学习如何使用YAML是非常有益的。

## 如何

首先，我们需要确保我们已经安装了PyYAML库。在Python中，我们可以通过以下命令来安装它：

```
pip install pyyaml
```

接下来，我们需要导入PyYAML库：

```
import yaml
```

现在，让我们看一个简单的例子来创建一个YAML文件。假设我们想要存储一些关于一个人的信息，如姓名、年龄和职业，我们可以使用YAML来做到这一点：

```
person:
  name: John Smith
  age: 30
  occupation: Software Developer
```

我们可以使用PyYAML库中的dump方法来将这些信息写入一个YAML文件：

```
with open('person.yaml', 'w') as file:
    yaml.dump(person, file)
```

通过加载这个文件，我们可以读取和访问这些信息：

```
with open('person.yaml') as file:
    person_info = yaml.load(file, Loader=yaml.FullLoader)
    print(person_info['name']) # 输出：John Smith
    print(person_info['age']) # 输出：30
    print(person_info['occupation']) # 输出：Software Developer
```

如果需要创建一个包含多个对象的YAML文件，可以使用字典嵌套列表的形式来做到这一点：

```
employees:
  - name: John Smith
    age: 30
    occupation: Software Developer
  - name: Jane Doe
    age: 28
    occupation: Data Analyst
```

## 深入探讨

在深入了解YAML之前，建议您先熟悉一下其基础语法和常见用法。一些额外的资源如下所示：

- [YAML官方文档](https://yaml.org/)
- [PyYAML官方文档](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [Learn X in Y minutes: YAML](https://learnxinyminutes.com/docs/zh-cn/yaml-cn/)
- [YAML语言入门教程](https://www.ruanyifeng.com/blog/2016/07/yaml.html)

## 请参阅

- [JSON vs XML vs YAML: Which Data Interchange Format Is Better?](https://blog.rapidapi.com/json-vs-xml-vs-yaml-which-data-interchange-format-is-the-best-to-use/)
- [YAML Syntax Cheat Sheet](https://yaml.org/refcard.html)
- [How to Use YAML for Configuration Files in Python Applications](https://www.digitalocean.com/community/tutorials/how-to-use-yaml-for-configuration-files-in-python-applications)