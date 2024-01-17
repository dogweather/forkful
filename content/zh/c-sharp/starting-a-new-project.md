---
title:                "开始一个新项目"
html_title:           "C#: 开始一个新项目"
simple_title:         "开始一个新项目"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么是新项目 & 为什么要开始一个新项目？

开始一个新项目是指在编程中创建一个全新的项目，也就是说，新的需求或者新的想法需要编写新的代码，以满足客户的需求或者实现新的功能。程序员开始一个新项目是为了满足客户的需求，或者为了改善现有系统，加入新的特性。

## 如何开始一个新项目？

```C#
// 1. 创建一个新的解决方案
dotnet new sln -n MyProject
// 2. 在解决方案中创建项目
dotnet new console -n MyFirstProject
// 3. 将项目添加到解决方案中
dotnet sln add MyFirstProject/MyFirstProject.csproj
// 4. 在“Program.cs”文件中编写你的代码
static void Main(string[] args) 
{
    Console.WriteLine("Hello World!");
}
// 5. 运行项目
dotnet run MyFirstProject/MyFirstProject.csproj
```

输出结果：
```
Hello World!
```

## 深入了解

创建新项目的一个常见替代方法是使用集成开发环境（IDE），例如Visual Studio，它可以自动完成上述步骤。另外，程序员可以使用现有的代码库作为新项目的基础，以便节省时间和精力。

此外，程序员可以选择不同的项目类型，例如ASP.NET Core Web应用程序或Xamarin移动应用程序。每种类型都有独特的功能和用途，程序员可以根据项目需求选择适合的类型。

## 查看更多

了解如何使用C#和.NET创建新项目，请参阅以下文档：

- [C#官方文档](https://docs.microsoft.com/zh-cn/dotnet/csharp/)
- [Visual Studio官方文档](https://docs.microsoft.com/zh-cn/visualstudio/)
- [ASP.NET Core官方文档](https://docs.microsoft.com/zh-cn/aspnet/core/?view=aspnetcore-3.1)
- [Xamarin官方文档](https://docs.microsoft.com/zh-cn/xamarin/)