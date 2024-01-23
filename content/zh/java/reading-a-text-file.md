---
title:                "阅读文本文件"
date:                  2024-01-20T17:54:30.630734-07:00
model:                 gpt-4-1106-preview
simple_title:         "阅读文本文件"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
阅读文本文件就是让程序从中抓取数据。程序员这么做是为了从存储资料、配置到输入数据的一系列应用需求。

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
