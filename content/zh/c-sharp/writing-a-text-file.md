---
title:                "编写文本文件"
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
写文本文件是输出数据到可读文件的过程。程序员这么做为了保存数据，日志记录，或是跨会话共享信息。

## How to:
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string path = @"C:\example\myfile.txt";
        string text = "欢迎使用C#！";

        // 写入文件
        File.WriteAllText(path, text);
        
        // 附加文本
        File.AppendAllText(path, "\n这是新加的一行。");

        // 读取并显示内容
        string readText = File.ReadAllText(path);
        Console.WriteLine(readText);
    }
}
```
输出:
```
欢迎使用C#！
这是新加的一行。
```

## Deep Dive
最初，文本文件是用打字机或遥控打字系统创建的，后来开发了计算机技术进行数字化处理。C# 提供了几个内置的类，如 `File` 和 `StreamWriter`，用于文本写入。`File.WriteAllText` 和 `File.AppendAllText` 是简单操作，但大文件或频繁写入时，使用 `StreamWriter` 可获更好的性能和控制。

## See Also
- C# 教程：[文件处理](https://www.tutorialspoint.com/csharp/csharp_file_io.htm)
- Stack Overflow：文件操作的最佳实践讨论