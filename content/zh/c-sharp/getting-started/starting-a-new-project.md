---
date: 2024-01-20 18:03:23.018426-07:00
description: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE\u5C31\u662F\u521B\u5EFA\u4E00\
  \u4E2A\u7A7A\u767D\u7684\u7F16\u7A0B\u73AF\u5883\uFF0C\u5F00\u59CB\u6784\u5EFA\u7A0B\
  \u5E8F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5B9E\u73B0\u4E00\
  \u4E2A\u7279\u5B9A\u7684\u76EE\u6807\uFF0C\u89E3\u51B3\u4E00\u4E2A\u95EE\u9898\u6216\
  \u8005\u628A\u60F3\u6CD5\u53D8\u6210\u73B0\u5B9E\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.768233-06:00'
model: gpt-4-1106-preview
summary: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE\u5C31\u662F\u521B\u5EFA\u4E00\
  \u4E2A\u7A7A\u767D\u7684\u7F16\u7A0B\u73AF\u5883\uFF0C\u5F00\u59CB\u6784\u5EFA\u7A0B\
  \u5E8F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5B9E\u73B0\u4E00\
  \u4E2A\u7279\u5B9A\u7684\u76EE\u6807\uFF0C\u89E3\u51B3\u4E00\u4E2A\u95EE\u9898\u6216\
  \u8005\u628A\u60F3\u6CD5\u53D8\u6210\u73B0\u5B9E\u3002."
title: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE"
weight: 1
---

## What & Why? (是什么？为什么？)
开始一个新项目就是创建一个空白的编程环境，开始构建程序。程序员这么做是为了实现一个特定的目标，解决一个问题或者把想法变成现实。

## How to: (如何操作：)
在C#中，开始新项目通常意味着使用.NET Core或.NET 5/6/7（取决于你的需求）。可以通过命令行或者IDE（例如Visual Studio）来创建。

命令行示例:
```csharp
// 创建一个新的控制台应用项目
dotnet new console -n MyNewApp
// 进入项目目录
cd MyNewApp
// 运行应用
dotnet run
```
输出示例:
```
Hello World!
```

## Deep Dive (深入了解)
.NET Core是一种跨平台的开发框架，允许你创建Windows、Mac和Linux应用程序。不同于过去的.NET Framework，.NET Core是开源的，并且支持更广泛的应用类型。

以前，C#开发主要集中在Windows系统，采用.NET Framework。随着.NET Core的出现，这种情况改变了。现在开发者有了更多的灵活性和选择。

除了.NET Core，还有Mono这样的替代框架，可以为不同操作系统创建应用，并且在.NET Core发布前就已经存在。

实际实施时，选择的框架要根据项目需求、团队技能以及目标平台来定。创建新项目时，了解各个框架的特点和局限性至关重要。

## See Also (另请参见)
- [.NET官方文档](https://docs.microsoft.com/zh-cn/dotnet/core/)
- [Visual Studio官方网站](https://visualstudio.microsoft.com/)
- [C#编程指南](https://docs.microsoft.com/zh-cn/dotnet/csharp/)
- [Mono项目](https://www.mono-project.com/)
