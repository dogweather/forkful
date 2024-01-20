---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么与为什么？

打印调试输出指的是将程序运行中的变量值、状态等详细信息输出，供开发者查看。程序员这么做是为了更好地理解程序运行过程，发现并修复错误。

## 如何做：

在C#中，我们常使用`Debug.WriteLine`来打印调试输出：

```C#
using System.Diagnostics;

public class Program
{
    public static void Main()
    {
        Debug.WriteLine("This is debug output");
        var myVariable = 5;
        Debug.WriteLine("The value of myVariable is: " + myVariable);
    }
}
```

运行此代码，将在调试窗口看到这样的输出：

```
This is debug output
The value of myVariable is: 5
```

## 深入探究

`Debug.WriteLine`始于.NET Framework, 它为开发者提供了一种方便快捷的方式来查看程序的信息。这并不是唯一的一种方法，我们还可以使用`Console.WriteLine`写入控制台或是利用日志框架（例如log4net）来输出信息。

实现细节方面，要注意`Debug.WriteLine`只在Debug版本的程序中有效，即你需要在项目属性设置中将active configuration 设置为Debug。在Release模式下，`Debug.WriteLine`的调用会被编译器忽略。

##  参阅

1. Microsoft的Debug类文档：[https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug?view=net-5.0](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug?view=net-5.0)
2. Debugging in Visual Studio: [https://docs.microsoft.com/en-us/visualstudio/debugger/?view=vs-2019](https://docs.microsoft.com/en-us/visualstudio/debugger/?view=vs-2019)