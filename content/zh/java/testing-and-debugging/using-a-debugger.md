---
date: 2024-01-26 03:50:05.607317-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u5047\u8BBE\u4F60\u6709\u4E00\u4E2A\u7B80\
  \u5355\u7684 Java \u7A0B\u5E8F\u51FA\u4E86\u95EE\u9898\uFF0C\u4F60\u641E\u4E0D\u6E05\
  \u695A\u4E3A\u4EC0\u4E48\u3002\u4EE5\u4E0B\u662F\u4F7F\u7528 Eclipse\uFF08\u4E00\
  \u4E2A\u6D41\u884C\u7684 Java \u5F00\u53D1 IDE\uFF09\u542F\u52A8\u8C03\u8BD5\u5668\
  \u7684\u65B9\u6CD5\uFF1A \u9996\u5148\uFF0C\u786E\u4FDD\u4F60\u5DF2\u7ECF\u8BBE\u7F6E\
  \u4E86\u4E00\u4E2A\u65AD\u70B9\u3002\u7136\u540E\uFF0C\u53F3\u952E\u70B9\u51FB\u6587\
  \u4EF6\uFF0C\u9009\u62E9\u201CDebug As\u201D\uFF0C\u5E76\u70B9\u51FB\u201CJava Application\u201D\
  \u3002"
lastmod: '2024-03-13T22:44:47.630349-06:00'
model: gpt-4-0125-preview
summary: "\u5047\u8BBE\u4F60\u6709\u4E00\u4E2A\u7B80\u5355\u7684 Java \u7A0B\u5E8F\
  \u51FA\u4E86\u95EE\u9898\uFF0C\u4F60\u641E\u4E0D\u6E05\u695A\u4E3A\u4EC0\u4E48\u3002\
  \u4EE5\u4E0B\u662F\u4F7F\u7528 Eclipse\uFF08\u4E00\u4E2A\u6D41\u884C\u7684 Java\
  \ \u5F00\u53D1 IDE\uFF09\u542F\u52A8\u8C03\u8BD5\u5668\u7684\u65B9\u6CD5\uFF1A\n\
  \n\u9996\u5148\uFF0C\u786E\u4FDD\u4F60\u5DF2\u7ECF\u8BBE\u7F6E\u4E86\u4E00\u4E2A\
  \u65AD\u70B9\u3002\u7136\u540E\uFF0C\u53F3\u952E\u70B9\u51FB\u6587\u4EF6\uFF0C\u9009\
  \u62E9\u201CDebug As\u201D\uFF0C\u5E76\u70B9\u51FB\u201CJava Application\u201D."
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

## 如何操作:
假设你有一个简单的 Java 程序出了问题，你搞不清楚为什么。以下是使用 Eclipse（一个流行的 Java 开发 IDE）启动调试器的方法：

首先，确保你已经设置了一个断点。然后，右键点击文件，选择“Debug As”，并点击“Java Application”。

```Java
public class DebugExample {
    public static void main(String[] args) {
        int a = 5;
        int b = 0;
        // 在这里设置一个断点
        int result = divide(a, b);
        System.out.println("结果是：" + result);
    }

    private static int divide(int numerator, int denominator) {
        // 另一个好的断点位置
        return numerator / denominator;
    }
}
```

这样做，你的程序将在断点处暂停，你可以检查变量，逐行执行代码，并观察你的程序行为。

在调试器控制台中的示例输出：
```
在行处触发断点：int result = divide(a, b);
```

## 深入了解
调试的概念自编程早期以来就已存在。传说中，“bug”这个术语实际上来自 Grace Hopper（该领域的先驱）在计算机内部发现的一只真实的天蛾虫子。快进到今天，我们拥有像 IntelliJ IDEA、Eclipse 和 NetBeans 这样的先进 IDE，它们配备了强大的调试器。

IDE调试器的替代方法包括日志记录、打印语句（穷人的调试器）、断言，以及独立的调试工具，如 jdb（Java 调试器），它是 Java 开发工具包 (JDK) 的一部分。

调试器的工作原理是允许程序员暂停执行（断点），逐步执行代码，检查变量值，即时修改这些值，甚至可以逐块运行代码。使用调试器通常被认为是开发复杂应用程序的一种宝贵技巧，其中追踪导致问题的确切代码行就像在大海捞针一样。

## 参见
- Oracle 官方调试文档：[Oracle Java SE Debugging](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/jdb.html)
- Eclipse 关于调试的指南：[Eclipse Debugging Tips](https://www.eclipse.org/community/eclipse_newsletter/2017/june/article4.php)
- VisualVM，一个集成了几个命令行 JDK 工具和轻量级性能分析功能的可视化工具：[VisualVM](https://visualvm.github.io/)
