---
title:                "使用关联数组"
aliases: - /zh/powershell/using-associative-arrays.md
date:                  2024-01-30T19:12:34.579955-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用关联数组"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

关联数组，在 PowerShell 中也被称为哈希表或字典，允许您以键值对的方式存储数据，使得数据检索直接且高效。程序员使用它们来存储相关数据，这样可以通过键轻松访问。

## 如何操作：

在 PowerShell 中创建和使用关联数组非常直接了当。以下是如何进行的操作：

**创建关联数组：**

```PowerShell
$myAssociativeArray = @{}
$myAssociativeArray["name"] = "Alex"
$myAssociativeArray["age"] = 25
$myAssociativeArray["job"] = "工程师"
```

这段代码片段创建了一个有三个键值对的关联数组。

**访问值：**

要获取一个值，请引用其键：

```PowerShell
Write-Output $myAssociativeArray["name"]
```

**示例输出：**

```
Alex
```

**添加或修改数据：**

只需使用键来添加新对或修改现有对：

```PowerShell
$myAssociativeArray["location"] = "纽约" # 添加一个新的键值对
$myAssociativeArray["job"] = "高级工程师" # 修改现有的键值对
```

**遍历关联数组：**

像这样循环遍历键和值：

```PowerShell
foreach ($key in $myAssociativeArray.Keys) {
  $value = $myAssociativeArray[$key]
  Write-Output "$key : $value"
}
```

**示例输出：**

```
name : Alex
age : 25
job : 高级工程师
location : 纽约
```

## 深入了解

关联数组的概念在许多编程语言中都很常见，根据语言的不同，通常称为字典、映射或哈希表。在 PowerShell 中，关联数组是以哈希表的形式实现的，这对于查找键、存储数据和维护唯一键的集合非常有效。

从历史上看，关联数组提供了一种管理对象集合的方法，其中每个项目都可以使用其键快速检索，而无需遍历整个集合。关联数组在数据检索和修改方面的效率使它们成为各种任务的首选。然而，它们确实有限制，比如维持顺序，对于这些有序字典或自定义对象可能是更好的选择。

尽管有这些限制，PowerShell 中的关联数组/哈希表非常灵活且是脚本编写的强大工具。它们允许动态数据存储，并且在配置、数据操作以及任何需要结构化数据格式但又不需要正式类定义的地方特别有用。只需记住，虽然关联数组非常适合基于键的检索，但如果您的任务涉及复杂的数据结构或需要维护特定顺序，您可能想要探索 PowerShell 中的其他数据类型或自定义对象。
