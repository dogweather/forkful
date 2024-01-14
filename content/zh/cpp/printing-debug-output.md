---
title:                "C++: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

为什么：为什么会使用打印调试输出？因为打印调试输出是帮助程序员调试代码的重要工具，它可以帮助我们发现错误并进行修复。

如何：首先，我们需要在代码中插入打印语句，使用cout语句可以打印出特定的变量值，如下所示：
```C++
int a = 10;
cout << "a的值为：" << a << endl;
```
输出结果为：“a的值为：10”。同时，还可以使用debug宏来控制打印语句的显示，如下所示：
```C++
#define DEBUG 1
int a = 10;
if (DEBUG) {
  cout << "a的值为：" << a << endl;
}
```
当DEBUG为1时，打印语句会显示在控制台上，当DEBUG为0时，打印语句则会被忽略。这样可以在需要调试时打印输出，而在正式运行时则不会有额外的输出。

深入探讨：打印调试输出的另一个重要作用是帮助我们理解程序的执行过程。通过观察打印输出的结果，我们可以更容易地理解程序的运行逻辑，从而更容易定位错误。另外，打印调试输出也可以帮助我们优化代码，比如可以打印出循环或递归过程中的变量值，从而找出可能存在的性能问题。

另请参阅：

- [C++调试技巧](https://blog.csdn.net/qlciou/article/details/13036135)
- [使用cout语句输出调试信息](https://www.cnblogs.com/52php/p/3609664.html)
- [使用debug宏来控制打印输出](https://www.lkmake.com/2017/05/04/cpp-print-debug-view/index.html)

参阅：

有用的链接：

- [Markdown基本语法](https://guides.github.com/features/mastering-markdown/)
- [C++参考手册](https://msdn.microsoft.com/zh-cn/library/wzxaf3ay.aspx)