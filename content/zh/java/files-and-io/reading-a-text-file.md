---
date: 2024-01-20 17:54:30.630734-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C) \u5386\u53F2\u4E0A\uFF0C\u6700\u521D\
  \u7684Java\u7248\u672C\u4F7F\u7528`FileInputStream`\u548C`BufferedReader`\u8FDB\u884C\
  \u6587\u4EF6\u8BFB\u53D6\u3002\u968F\u7740Java\u7684\u8FDB\u5316\uFF0C`java.nio`\u5305\
  \u63D0\u4F9B\u4E86\u66F4\u9AD8\u6548\u3001\u66F4\u7B80\u6D01\u7684API\uFF0C\u5982\
  `Files`\u7C7B\u3002 \u66FF\u4EE3\u65B9\u6CD5\u5305\u62EC`Scanner`\u7C7B\u548C\u7B2C\
  \u4E09\u65B9\u5E93\u5982Apache Commons\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.850913-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C) \u5386\u53F2\u4E0A\uFF0C\u6700\u521D\u7684Java\u7248\
  \u672C\u4F7F\u7528`FileInputStream`\u548C`BufferedReader`\u8FDB\u884C\u6587\u4EF6\
  \u8BFB\u53D6\u3002\u968F\u7740Java\u7684\u8FDB\u5316\uFF0C`java.nio`\u5305\u63D0\
  \u4F9B\u4E86\u66F4\u9AD8\u6548\u3001\u66F4\u7B80\u6D01\u7684API\uFF0C\u5982`Files`\u7C7B\
  \u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

## How to: (如何操作)
```Java
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class FileReaderExample {
    public static void main(String[] args) {
        try {
            String content = Files.readString(Path.of("example.txt"));
            System.out.println(content);
        } catch (IOException e) {
            System.out.println("Error reading the file.");
        }
    }
}
```

输出样例 (Sample Output):
```
这是一个文本文件的内容示例。
第二行的文本。
```

## Deep Dive (深入探索)
历史上，最初的Java版本使用`FileInputStream`和`BufferedReader`进行文件读取。随着Java的进化，`java.nio`包提供了更高效、更简洁的API，如`Files`类。

替代方法包括`Scanner`类和第三方库如Apache Commons IO。

实现细节方面，结合异常处理非常重要，因为文件读取可能因为文件不存在、权限不足或其他I/O问题而失败。

## See Also (另请参阅)
- [Oracle官方文档 - Files.readString](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#readString(java.nio.file.Path))
- [Oracle官方教程 - 读取、写入和创建文件](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Apache Commons IO](https://commons.apache.org/proper/commons-io/)
