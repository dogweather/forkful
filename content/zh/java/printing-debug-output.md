---
title:                "打印调试输出"
date:                  2024-01-20T17:52:57.784685-07:00
model:                 gpt-4-1106-preview
simple_title:         "打印调试输出"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
打印调试输出就是在程序中输出信息来追踪运行情况。程序员这样做是为了在开发时快速发现和修复问题。

## How to: (如何操作：)
```java
public class DebugExample {
    public static void main(String[] args) {
        int sum = 0;
        for (int i = 0; i < 5; i++) {
            sum += i;
            System.out.println("i=" + i + ", sum=" + sum); // 这是一个调试信息
        }
    }
}
```
输出：
```
i=0, sum=0
i=1, sum=1
i=2, sum=3
i=3, sum=6
i=4, sum=10
```

## Deep Dive (深入了解)
在Java早期版本中，`System.out.println()`是输出调试信息的常见方法。但是，它不适宜于大型或多线程应用。现在有许多日志库如Log4j或SLF4J提供了更丰富的日志管理功能。这些库允许你控制日志级别和输出格式，也更容易集成到现代监控系统中。

日志级别通常包括：ERROR, WARN, INFO, DEBUG, 和 TRACE。使用DEBUG或TRACE级别往往有助于深入问题，而INFO以上级别则用于生产环境中的重要信息。

实施时，你可能会用到条件语句避免在生产环境输出过多信息。例如，只有当你的应用在调试模式时，才会打印调试信息。

## See Also (另请参阅)
- Oracle Java Documentation: https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html
- Log4j 2: https://logging.apache.org/log4j/2.x/
- SLF4J Project: http://www.slf4j.org/
- Effective Java (书籍), Joshua Bloch: 提供了关于Java编程的最佳实践。