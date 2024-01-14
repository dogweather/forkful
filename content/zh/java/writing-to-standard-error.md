---
title:    "Java: 写入标准错误"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

很多人在编程的时候可能会遇到写到标准错误的问题，那么为什么要这么做呢？其实，写到标准错误是一种错误处理的方式，它可以帮助我们更好地定位和解决程序中出现的问题。

## 如何进行

首先，我们需要使用Java中的System类来进行写到标准错误。我们可以通过调用System.err.println()方法来实现。下面是一个简单的例子：

```Java
public class WriteToStandardErrorDemo {
  public static void main(String[] args) {
    System.err.println("This message will be printed to the standard error stream.");
  }
}
```

运行上面的代码，将会在控制台中打印出相应的信息。

## 深入了解

标准错误是一个输出流，它用于输出程序中的错误信息。与标准输出类似，它也可以被重定向到其他地方，比如文件中。此外，我们还可以将它与标准输出流结合起来，通过System.setErr()方法来设置。

写到标准错误也是很有用的，比如在我们开发网络应用程序时，我们可以将错误信息输出到服务器日志文件中，方便我们及时发现和解决问题。此外，当我们与其他程序共同使用标准错误流时，可以更好地获取程序间的通信信息。

## 参考资料

如果你想了解更多关于Java中写到标准错误的相关知识，可以参考以下链接：

- [Java System class (English)](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Java I/O (中文)](https://www.runoob.com/java/java-files-io.html)
- [Java标准输出与标准错误解释 (中文)](https://juejin.im/post/5baeef0de51d450e7245e9e0)
- [Java System类详解 (中文)](https://www.jianshu.com/p/29919c8dfaac)

## 参见

- [关于Java标准输出 (中文)](https://github.com/Kayoxity/Java-System.out-println-Tutorial/blob/master/README.md)