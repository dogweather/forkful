---
date: 2024-01-26 04:20:23.582566-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u9996\u5148\uFF0C\u5B89\u88C5\u4E00\u4E2A\
  TOML\u89E3\u6790\u5668\uFF0C\u6BD4\u5982`Tomlyn`\u3002\u4F7F\u7528\u4F60\u7684\u5305\
  \u7BA1\u7406\u5668\uFF1A."
lastmod: '2024-04-05T21:53:48.107286-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528TOML"
weight: 39
---

## 如何操作：
首先，安装一个TOML解析器，比如`Tomlyn`。使用你的包管理器：

```csharp
dotnet add package Tomlyn
```

接下来，解析一个TOML文件：

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Owner: {tomlTable["owner"]["name"]}");
// 输出：
// Owner: Tom Preston-Werner
```

现在，创建并写入TOML：

```csharp
using Tomlyn;
using Tomlyn.Syntax;
using System;
using System.IO;

var doc = new DocumentSyntax
{
    Tables =
    {
        new TableSyntax("owner")
        {
            Items =
            {
                { "name", "Tom Preston-Werner" },
                { "dob", "1979-05-27T07:32:00Z" }
            }
        }
    }
};

var tomlString = doc.ToString();
File.WriteAllText("config.toml", tomlString);
Console.WriteLine("TOML已写入config.toml");
// 输出：
// TOML已写入config.toml
```

## 深入了解：
TOML是由GitHub的联合创始人Tom Preston-Werner在2013年左右创建的，是对现有格式如YAML和JSON在配置设置中的局限性的一种反应。它专为配置而设计，强调直接和无歧义。

其他配置格式包括YAML、JSON和XML。然而，TOML因其对手动编辑配置文件更友好而脱颖而出。JSON虽然无处不在，但对于复杂配置的可读性较差，而XML则过于冗长。尽管YAML在可读性上与之类似，但是它对空格的大量使用可能会变得复杂，并且在某些内容上存在安全风险。

在实现方面，TOML专注于清晰地映射到哈希表上，使数据提取可预测。随着1.0.0版本的发布，TOML巩固了其规范，提高了稳定性和工具支持。

## 另请参见：
- 官方TOML GitHub仓库和规范：[github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn，.NET库：[github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
