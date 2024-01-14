---
title:                "Java: 向标准错误写入"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

#为什么

写入标准错误是一种重要的编程技术，它可以帮助程序员在调试和错误处理中更有效地识别问题。通过写入和打印错误信息，程序员可以迅速地找出造成问题的源头，并进行相应的调整和修复。

#如何操作

在Java中，我们可以通过使用System.err.println（）来编写标准错误。这个方法可以将指定的字符串写入标准错误流中，并将其打印到控制台。下面是一个简单的示例代码：

```Java 
public class ErrorWriter {
    public static void main(String[] args) {
        int num = 10 / 0; // 故意制造一个除零错误

        System.err.println("出错啦！"); // 将错误信息写入标准错误流
    }
}
```

执行上述代码后，我们可以在控制台中看到如下输出：

```Java 
Exception in thread "main" java.lang.ArithmeticException: / by zero
    at ErrorWriter.main(ErrorWriter.java:4)
出错啦！
```

可以看到除零错误的具体信息被写入了标准错误流，而我们在代码中写入的 "出错啦！" 也被打印出来了。

#深入探讨

除了可以将错误信息打印到控制台，我们还可以通过重定向标准错误流来实现其他功能。比如，我们可以将错误信息写入日志文件，或者将其发送给指定的错误处理系统。

在Java中，我们可以使用System.setErr（）方法来重定向标准错误流。下面是一个简单的示例代码：

```Java 
public class ErrorRedirector {
    public static void main(String[] args) {
        try {
            int num = 10 / 0; // 故意制造一个除零错误
        } catch (ArithmeticException e) {
            // 将标准错误流重定向到指定的日志文件
            System.setErr(new PrintStream(new FileOutputStream("error.log")));
            // 将错误信息写入标准错误流
            System.err.println(e.getMessage());
        }
    }
}
```

执行上述代码后，除零错误的信息就会被写入到指定的日志文件中。

#另请参阅

- [Java官方文档：System类](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Java官方文档：PrintStream类](https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html)
- [控制台输出和标准错误输出到文件](https://blog.csdn.net/myarrow/article/details/4647474)