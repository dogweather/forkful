---
date: 2024-01-20 17:52:57.784685-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728Java\u65E9\u671F\u7248\
  \u672C\u4E2D\uFF0C`System.out.println()`\u662F\u8F93\u51FA\u8C03\u8BD5\u4FE1\u606F\
  \u7684\u5E38\u89C1\u65B9\u6CD5\u3002\u4F46\u662F\uFF0C\u5B83\u4E0D\u9002\u5B9C\u4E8E\
  \u5927\u578B\u6216\u591A\u7EBF\u7A0B\u5E94\u7528\u3002\u73B0\u5728\u6709\u8BB8\u591A\
  \u65E5\u5FD7\u5E93\u5982Log4j\u6216SLF4J\u63D0\u4F9B\u4E86\u66F4\u4E30\u5BCC\u7684\
  \u65E5\u5FD7\u7BA1\u7406\u529F\u80FD\u3002\u8FD9\u4E9B\u5E93\u5141\u8BB8\u4F60\u63A7\
  \u5236\u65E5\u5FD7\u7EA7\u522B\u548C\u8F93\u51FA\u683C\u5F0F\uFF0C\u4E5F\u66F4\u5BB9\
  \u6613\u96C6\u6210\u5230\u73B0\u4EE3\u76D1\u63A7\u7CFB\u7EDF\u4E2D\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.835160-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728Java\u65E9\u671F\u7248\u672C\u4E2D\
  \uFF0C`System.out.println()`\u662F\u8F93\u51FA\u8C03\u8BD5\u4FE1\u606F\u7684\u5E38\
  \u89C1\u65B9\u6CD5\u3002\u4F46\u662F\uFF0C\u5B83\u4E0D\u9002\u5B9C\u4E8E\u5927\u578B\
  \u6216\u591A\u7EBF\u7A0B\u5E94\u7528\u3002\u73B0\u5728\u6709\u8BB8\u591A\u65E5\u5FD7\
  \u5E93\u5982Log4j\u6216SLF4J\u63D0\u4F9B\u4E86\u66F4\u4E30\u5BCC\u7684\u65E5\u5FD7\
  \u7BA1\u7406\u529F\u80FD\u3002\u8FD9\u4E9B\u5E93\u5141\u8BB8\u4F60\u63A7\u5236\u65E5\
  \u5FD7\u7EA7\u522B\u548C\u8F93\u51FA\u683C\u5F0F\uFF0C\u4E5F\u66F4\u5BB9\u6613\u96C6\
  \u6210\u5230\u73B0\u4EE3\u76D1\u63A7\u7CFB\u7EDF\u4E2D\u3002"
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

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
