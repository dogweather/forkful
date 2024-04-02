---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:10.548851-07:00
description: "\u5728Java\u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u662F\u5173\u4E8E\
  \u4F7F\u7528\u8BED\u8A00\u7684\u80FD\u529B\u6765\u521B\u5EFA\u548C\u5199\u5165\u6587\
  \u4EF6\u7CFB\u7EDF\u4E2D\u7684\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u51FA\u4E8E\u5404\
  \u79CD\u539F\u56E0\u8FD9\u6837\u505A\uFF0C\u5982\u65E5\u5FD7\u8BB0\u5F55\u3001\u5BFC\
  \u51FA\u6570\u636E\u6216\u4E3A\u4E86\u4EE5\u540E\u68C0\u7D22\u4FDD\u5B58\u5E94\u7528\
  \u7A0B\u5E8F\u72B6\u6001\u3002"
lastmod: '2024-03-13T22:44:47.645538-06:00'
model: gpt-4-0125-preview
summary: "\u5728Java\u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u662F\u5173\u4E8E\u4F7F\
  \u7528\u8BED\u8A00\u7684\u80FD\u529B\u6765\u521B\u5EFA\u548C\u5199\u5165\u6587\u4EF6\
  \u7CFB\u7EDF\u4E2D\u7684\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u51FA\u4E8E\u5404\u79CD\
  \u539F\u56E0\u8FD9\u6837\u505A\uFF0C\u5982\u65E5\u5FD7\u8BB0\u5F55\u3001\u5BFC\u51FA\
  \u6570\u636E\u6216\u4E3A\u4E86\u4EE5\u540E\u68C0\u7D22\u4FDD\u5B58\u5E94\u7528\u7A0B\
  \u5E8F\u72B6\u6001\u3002"
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
weight: 24
---

## 什么与为什么？

在Java中写入文本文件是关于使用语言的能力来创建和写入文件系统中的文件。程序员出于各种原因这样做，如日志记录、导出数据或为了以后检索保存应用程序状态。

## 如何操作：

### 使用 `java.nio.file`（标准库）

Java的新I/O（NIO）包（`java.nio.file`）为处理文件提供了更多样化的方法。以下是使用`Files.write()`写入文件的简化方式：

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> lines = Arrays.asList("第一行", "第二行", "第三行");
        try {
            Files.write(Paths.get("example.txt"), lines);
            System.out.println("文件成功写入！");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

输出：

```
文件成功写入！
```

### 使用 `java.io`（标准库）

对于更传统的方法，`java.io.FileWriter`是写入文本文件的一个好选择：

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("example.txt")) {
            writer.write("你好，世界！\n");
            writer.append("这是另一行。");
            System.out.println("文件成功写入！");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

输出：

```
文件成功写入！
```

### 使用 Apache Commons IO

Apache Commons IO库简化了许多操作，包括文件写入。以下是使用`FileUtils.writeStringToFile()`写入文件的方式：

首先，向您的项目添加依赖。如果使用Maven，请包括：

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- 检查最新版本 -->
</dependency>
```

然后，使用以下代码写入文本文件：

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("example.txt"), "这是使用Commons IO写入的文本。", "UTF-8");
            System.out.println("文件成功写入！");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

输出：

```
文件成功写入！
```
