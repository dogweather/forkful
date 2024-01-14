---
title:    "C#: 创建临时文件"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

为什么：创建临时文件的原因
在C#编程中，有时候我们需要临时存储一些数据或者临时生成一些文件，这时候就可以用到临时文件。

怎么做：
```C#
// 在C#中创建临时文件的方法
string tempFileName = Path.GetTempFileName();
Console.WriteLine($"临时文件的名称是：{tempFileName}");
```
输出结果：
```
临时文件的名称是：C:\Users\UserName\AppData\Local\Temp\tmp1234.tmp 
```
深入了解：
临时文件通常是为了临时存储数据而创建的，当程序关闭后，这些临时文件就会自动删除。我们可以通过`Path.GetTempFileName()`方法来创建一个临时文件，并返回一个唯一的文件名。这个方法会在我们系统的临时文件夹中创建一个文件，这样我们就不用担心文件名的重复问题。此外，我们也可以通过设置`Path.GetTempFileName()`方法的参数自定义文件名。

另外，临时文件一般用于存储一些临时的数据，所以我们在使用完后最好手动删除，以免占用过多的空间。

另外，我们也可以通过`Path.GetTempPath()`方法来获取系统的临时文件夹的路径。

总结：
创建临时文件是一个很常见的操作，在C#中也提供了很多简便的方法来实现。当我们需要临时存储或者操作一些数据时，不妨尝试使用临时文件来帮助我们完成任务。

参考链接：
- [Path.GetTempFileName()方法官方文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.io.path.gettempfilename)
- [Path.GetTempPath()方法官方文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.io.path.gettemppath)
- [C#中临时文件的创建与使用](https://www.cnblogs.com/linuxAndMcu/p/3598234.html)

相关链接：
- [C#中文件操作的基本知识](https://www.cnblogs.com/linuxAndMcu/p/3538899.html)
- [C#中文件操作的高级用法](https://www.cnblogs.com/linuxAndMcu/p/3590110.html)