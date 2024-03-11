---
date: 2024-01-26 03:50:05.607317-07:00
description: "\u4F7F\u7528\u8C03\u8BD5\u5668\u610F\u5473\u7740\u5229\u7528\u5DE5\u5177\
  \u6765\u6D4B\u8BD5\u548C\u4FEE\u590D\u4EE3\u7801\u4E2D\u7684\u9519\u8BEF\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u7406\u89E3\u5E94\u7528\u7A0B\u5E8F\
  \u7684\u6D41\u7A0B\uFF0C\u51C6\u786E\u627E\u5230\u9519\u8BEF\u6E90\u5934\uFF0C\u5E76\
  \u9A8C\u8BC1\u6267\u884C\u4E2D\u7684\u903B\u8F91\u3002"
lastmod: '2024-03-11T00:14:21.404211-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u7528\u8C03\u8BD5\u5668\u610F\u5473\u7740\u5229\u7528\u5DE5\u5177\
  \u6765\u6D4B\u8BD5\u548C\u4FEE\u590D\u4EE3\u7801\u4E2D\u7684\u9519\u8BEF\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u7406\u89E3\u5E94\u7528\u7A0B\u5E8F\
  \u7684\u6D41\u7A0B\uFF0C\u51C6\u786E\u627E\u5230\u9519\u8BEF\u6E90\u5934\uFF0C\u5E76\
  \u9A8C\u8BC1\u6267\u884C\u4E2D\u7684\u903B\u8F91\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
---

{{< edit_this_page >}}

## 什么 & 为什么?
使用调试器意味着利用工具来测试和修复代码中的错误。程序员这么做是为了理解应用程序的流程，准确找到错误源头，并验证执行中的逻辑。

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
