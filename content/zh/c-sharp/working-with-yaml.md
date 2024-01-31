---
title:                "处理 YAML 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么?)
YAML是一种简洁的数据序列化格式，方便数据存储和传输。编程中用YAML是因为它易读易写，且被广泛应用于配置文件和数据交换。

## How to: (如何操作)
为处理YAML，我们先安装YamlDotNet库：

```bash
dotnet add package YamlDotNet
```

示例代码读取并解析YAML文件：

```C#
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Program
{
    public static void Main()
    {
        var yaml = @"
name: Zhang San
age: 30
languages:
  - C#
  - Python
";

        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();

        var person = deserializer.Deserialize<Person>(yaml);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        foreach(var lang in person.Languages)
        {
            Console.WriteLine($"Language: {lang}");
        }
    }
}

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    public List<string> Languages { get; set; }
}
```

输出结果：

```
Name: Zhang San, Age: 30
Language: C#
Language: Python
```

## Deep Dive (深入研究)
YAML在2001年出现，意为“YAML Ain't Markup Language”。其他数据序列化格式如JSON和XML，JSON更简洁但不支持注释，XML支持注释但较冗长。在C#中，我们通常使用YamlDotNet库处理YAML，它提供了强大的序列化和反序列化功能。

## See Also (另请参阅)
- YamlDotNet库官方文档: [https://github.com/aaubry/YamlDotNet/wiki](https://github.com/aaubry/YamlDotNet/wiki)
- YAML官方网站: [https://yaml.org/](https://yaml.org/)
- YAML和JSON对比: [https://stackoverflow.com/questions/1726802/what-is-the-difference-between-yaml-and-json](https://stackoverflow.com/questions/1726802/what-is-the-difference-between-yaml-and-json)
